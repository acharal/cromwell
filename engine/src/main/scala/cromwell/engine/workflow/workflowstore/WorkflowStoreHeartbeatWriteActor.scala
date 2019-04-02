package cromwell.engine.workflow.workflowstore

import java.time.{OffsetDateTime, Duration => JDuration}

import akka.actor.{ActorRef, CoordinatedShutdown, Props}
import cats.data.{NonEmptyList, NonEmptyVector}
import cromwell.core.Dispatcher.EngineDispatcher
import cromwell.core.WorkflowId
import cromwell.core.instrumentation.InstrumentationPrefixes
import cromwell.engine.CromwellTerminator
import cromwell.engine.workflow.workflowstore.WorkflowStoreActor.WorkflowStoreWriteHeartbeatCommand
import cromwell.services.EnhancedBatchActor
import mouse.all._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class WorkflowStoreHeartbeatWriteActor(workflowStoreAccess: WorkflowStoreAccess,
                                            workflowHeartbeatConfig: WorkflowHeartbeatConfig,
                                            terminator: CromwellTerminator,
                                            override val serviceRegistryActor: ActorRef)

  extends EnhancedBatchActor[(WorkflowId, OffsetDateTime)](
    flushRate = workflowHeartbeatConfig.heartbeatInterval,
    batchSize = workflowHeartbeatConfig.writeBatchSize) {

  override val threshold = workflowHeartbeatConfig.writeThreshold

  private val failureShutdownDurationOption = workflowHeartbeatConfig.failureShutdownDurationOption

  //noinspection ActorMutableStateInspection
  private var firstFailureOption: Option[OffsetDateTime] = None

  /**
    * Process the data asynchronously
    *
    * @return the number of elements processed
    */
  override protected def process(data: NonEmptyVector[(WorkflowId, OffsetDateTime)]): Future[Int] = instrumentedProcess {
    val processStart = OffsetDateTime.now()
    val processFuture = workflowStoreAccess.writeWorkflowHeartbeats(data)
    processFuture transform {
      // Track the `Try`, and then return the original `Try`. Similar to `andThen` but doesn't swallow exceptions.
      _ <| trackRepeatedFailures(processStart, data.length)
    }
  }

  override def receive = enhancedReceive.orElse(super.receive)
  override protected def weightFunction(command: (WorkflowId, OffsetDateTime)) = 1
  override protected def instrumentationPath = NonEmptyList.of("store", "heartbeat-writes")
  override protected def instrumentationPrefix = InstrumentationPrefixes.WorkflowPrefix
  override def commandToData(snd: ActorRef): PartialFunction[Any, (WorkflowId, OffsetDateTime)] = {
    case command: WorkflowStoreWriteHeartbeatCommand => (command.workflowId, command.submissionTime)
  }

  /*
  WARNING: Even though this is in an actor, the logic deals with instances of Future that could complete in _any_ order,
  and even call this method at the same time from different threads.

  We are expecting the underlying FSM to ensure that the call to this method does NOT occur in parallel, waiting for
  the call to `process` to complete.
   */
  private def trackRepeatedFailures(startTime: OffsetDateTime, workflowCount: Int)(processTry: Try[Int]): Unit = {
    processTry match {
      case Success(_) =>
        firstFailureOption = None
      case Failure(_: Exception) =>
        if (firstFailureOption.isEmpty) {
          firstFailureOption = Option(startTime)
        }

        for {
          failureShutdownDuration <- failureShutdownDurationOption
          firstFailure <- firstFailureOption
        } {
          val now = OffsetDateTime.now()
          val failureDuration = JDuration.between(firstFailure, now)
          if (failureDuration.toNanos > failureShutdownDuration.toNanos) {
            log.error(
              "More than {} of errors since trying to write heartbeats at {}. Shutting down {}",
              failureShutdownDuration,
              firstFailure,
              workflowHeartbeatConfig.cromwellId
            )
            terminator.beginCromwellShutdown(WorkflowStoreHeartbeatWriteActor.Shutdown)
          }
        }
        ()
      case Failure(throwable) => throw throwable
    }
  }

}

object WorkflowStoreHeartbeatWriteActor {
  object Shutdown extends CoordinatedShutdown.Reason

  def props(
             workflowStoreAccess: WorkflowStoreAccess,
             workflowHeartbeatConfig: WorkflowHeartbeatConfig,
             terminator: CromwellTerminator,
             serviceRegistryActor: ActorRef
           ): Props =
    Props(
      WorkflowStoreHeartbeatWriteActor(
        workflowStoreAccess = workflowStoreAccess,
        workflowHeartbeatConfig = workflowHeartbeatConfig,
        terminator = terminator,
        serviceRegistryActor = serviceRegistryActor
      )).withDispatcher(EngineDispatcher)
}
