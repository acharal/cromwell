package cromwell.backend.impl.tes

import cats.syntax.validated._
import cats.syntax.list._
import com.typesafe.config.Config
import common.validation.ErrorOr.ErrorOr
import cromwell.backend.standard.StandardValidatedRuntimeAttributesBuilder
import cromwell.backend.validation._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import wom.RuntimeAttributesKeys
import wom.format.MemorySize
import wdl4s.parser.MemoryUnit
import wom.values._

case class TesRuntimeAttributes(continueOnReturnCode: ContinueOnReturnCode,
                                dockerImage: String,
                                dockerWorkingDir: Option[String],
                                failOnStderr: Boolean,
                                cpu: Option[Int Refined Positive],
                                memory: Option[MemorySize],
                                disk: Option[MemorySize])

object TesRuntimeAttributes {

  val DockerWorkingDirKey = "dockerWorkingDir"
  val DiskSizeKey = "disk"

  private def cpuValidation(runtimeConfig: Option[Config]): OptionalRuntimeAttributesValidation[Int Refined Positive] = CpuValidation.optional

  private def failOnStderrValidation(runtimeConfig: Option[Config]) = FailOnStderrValidation.default(runtimeConfig)

  private def continueOnReturnCodeValidation(runtimeConfig: Option[Config]) = ContinueOnReturnCodeValidation.default(runtimeConfig)

  private def diskSizeValidation(runtimeConfig: Option[Config]): OptionalRuntimeAttributesValidation[MemorySize] = MemoryValidation.optional(DiskSizeKey)

  private def memoryValidation(runtimeConfig: Option[Config]): OptionalRuntimeAttributesValidation[MemorySize] = MemoryValidation.optional(RuntimeAttributesKeys.MemoryKey)

  private val dockerValidation: RuntimeAttributesValidation[String] = DockerValidation.instance

  private val dockerWorkingDirValidation: OptionalRuntimeAttributesValidation[String] = DockerWorkingDirValidation.optional

  private val outDirMinValidation: OptionalRuntimeAttributesValidation[MemorySize] = {
    InformationValidation.optional(RuntimeAttributesKeys.OutDirMinKey, MemoryUnit.MB, allowZero = true)
  }

  private val tmpDirMinValidation: OptionalRuntimeAttributesValidation[MemorySize] = {
    InformationValidation.optional(RuntimeAttributesKeys.TmpDirMinKey, MemoryUnit.MB, allowZero = true)
  }

  private val inputDirMinValidation: OptionalRuntimeAttributesValidation[MemorySize] = {
    InformationValidation.optional(RuntimeAttributesKeys.DnaNexusInputDirMinKey, MemoryUnit.MB, allowZero = true)
  }

  def runtimeAttributesBuilder(backendRuntimeConfig: Option[Config]): StandardValidatedRuntimeAttributesBuilder =
    StandardValidatedRuntimeAttributesBuilder.default(backendRuntimeConfig).withValidation(
      cpuValidation(backendRuntimeConfig),
      memoryValidation(backendRuntimeConfig),
      diskSizeValidation(backendRuntimeConfig),
      dockerValidation,
      dockerWorkingDirValidation,
      outDirMinValidation,
      tmpDirMinValidation,
      inputDirMinValidation
    )

  def apply(validatedRuntimeAttributes: ValidatedRuntimeAttributes, backendRuntimeConfig: Option[Config]): TesRuntimeAttributes = {
    val docker: String = RuntimeAttributesValidation.extract(dockerValidation, validatedRuntimeAttributes)
    val dockerWorkingDir: Option[String] = RuntimeAttributesValidation.extractOption(dockerWorkingDirValidation.key, validatedRuntimeAttributes)
    val cpu: Option[Int Refined Positive] = RuntimeAttributesValidation.extractOption(cpuValidation(backendRuntimeConfig).key, validatedRuntimeAttributes)
    val memory: Option[MemorySize] = RuntimeAttributesValidation.extractOption(memoryValidation(backendRuntimeConfig).key, validatedRuntimeAttributes)
    val disk: Option[MemorySize] = RuntimeAttributesValidation.extractOption(diskSizeValidation(backendRuntimeConfig).key, validatedRuntimeAttributes)
    val failOnStderr: Boolean =
      RuntimeAttributesValidation.extract(failOnStderrValidation(backendRuntimeConfig), validatedRuntimeAttributes)
    val continueOnReturnCode: ContinueOnReturnCode =
      RuntimeAttributesValidation.extract(continueOnReturnCodeValidation(backendRuntimeConfig), validatedRuntimeAttributes)

    val outDirMin: Option[MemorySize] = RuntimeAttributesValidation.extractOption(outDirMinValidation.key, validatedRuntimeAttributes)
    val tmpDirMin: Option[MemorySize] = RuntimeAttributesValidation.extractOption(tmpDirMinValidation.key, validatedRuntimeAttributes)
    val inputDirMin: Option[MemorySize] = RuntimeAttributesValidation.extractOption(inputDirMinValidation.key, validatedRuntimeAttributes)

    val totalExecutionDiskSizeBytes = List(inputDirMin.map(_.bytes), outDirMin.map(_.bytes), tmpDirMin.map(_.bytes))
      .flatten.toNel.map(ls => ls.toList.fold(MemorySize(0, MemoryUnit.Bytes).bytes)(_ + _))

    val totalExecutionDiskSize = totalExecutionDiskSizeBytes.map(b => MemorySize(b, MemoryUnit.Bytes))

    val adjustedDisk = List(disk, totalExecutionDiskSize)
      .flatten
      .toNel
      .map(ls => ls.toList.map(m => m.to(MemoryUnit.GB).amount).max)
      .map(b  => MemorySize(b, MemoryUnit.GB))

    new TesRuntimeAttributes(
      continueOnReturnCode,
      docker,
      dockerWorkingDir,
      failOnStderr,
      cpu,
      memory,
      adjustedDisk
    )
  }
}

object DockerWorkingDirValidation {
  lazy val instance: RuntimeAttributesValidation[String] = new DockerWorkingDirValidation
  lazy val optional: OptionalRuntimeAttributesValidation[String] = instance.optional
}

class DockerWorkingDirValidation extends StringRuntimeAttributesValidation(TesRuntimeAttributes.DockerWorkingDirKey) {
  // NOTE: Docker's current test specs don't like WdlInteger, etc. auto converted to WdlString.
  override protected def validateValue: PartialFunction[WomValue, ErrorOr[String]] = {
    case WomString(value) => value.validNel
  }
}
