/*
 * Copyright 2001-2023 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatestplus.junit5

import org.junit.platform.commons.support.ReflectionSupport
import org.junit.platform.engine.discovery.{ClassSelector, ClasspathRootSelector, ModuleSelector, PackageSelector, UniqueIdSelector}
import org.junit.platform.engine.support.descriptor.EngineDescriptor
import org.junit.platform.engine.support.discovery.SelectorResolver.{Match, Resolution}
import org.junit.platform.engine.support.discovery.{EngineDiscoveryRequestResolver, SelectorResolver}
import org.junit.platform.engine.{EngineDiscoveryRequest, ExecutionRequest, TestDescriptor, TestExecutionResult, UniqueId}
import org.scalatest.{Args, ConfigMap, DynaTags, Filter, Stopper, Tracker}

import java.lang.reflect.Modifier
import java.util.concurrent.atomic.AtomicInteger
import java.util.Optional
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors, ThreadFactory, TimeUnit}
import java.util.logging.Logger
import java.util.stream.Collectors
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.reflect.NameTransformer
import scala.util.{Failure, Success, Try}

/**
 * ScalaTest implementation for JUnit 5 Test Engine.
 */ 
class ScalaTestEngine extends org.junit.platform.engine.TestEngine {

  private val logger = Logger.getLogger(classOf[ScalaTestEngine].getName)

  /**
   * Test engine ID, return "scalatest".
   */
  def getId: String = "scalatest"

  /**
   * Discover ScalaTest suites, you can disable the discover by setting system property org.scalatestplus.junit5.ScalaTestEngine.disabled to "true".
   */
  def discover(discoveryRequest: EngineDiscoveryRequest, uniqueId: UniqueId): TestDescriptor = {
    // reference: https://blogs.oracle.com/javamagazine/post/junit-build-custom-test-engines-java
    //            https://software-matters.net/posts/custom-test-engine/

    val engineDesc = new EngineDescriptor(uniqueId, "ScalaTest EngineDescriptor")

    if (System.getProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled") != "true") {
      logger.fine("Starting test discovery...")

      val alwaysTruePredicate =
        new java.util.function.Predicate[String]() {
          def test(t: String): Boolean = true
        }

      val isSuitePredicate =
        new java.util.function.Predicate[Class[_]]() {
          def test(t: Class[_]): Boolean = 
            classOf[org.scalatest.Suite].isAssignableFrom(t) &&
            !Modifier.isAbstract(t.getModifiers) &&
            JUnitHelper.checkForPublicNoArgConstructor(t)
        }

      def classDescriptorFunction(aClass: Class[_]) =
        new java.util.function.Function[TestDescriptor, Optional[ScalaTestClassDescriptor]]() {
          def apply(parent: TestDescriptor): Optional[ScalaTestClassDescriptor] = {
            val suiteUniqueId = parent.getUniqueId.append(ScalaTestClassDescriptor.segmentType, aClass.getName)
            parent.getChildren.asScala.find(_.getUniqueId == suiteUniqueId) match {
              case Some(_) => Optional.empty[ScalaTestClassDescriptor]()
              case None => Optional.of(new ScalaTestClassDescriptor(engineDesc, suiteUniqueId, aClass, true))
            }
          }
        }

      val toMatch =
        new java.util.function.Function[TestDescriptor, java.util.stream.Stream[Match]]() {
          def apply(td: TestDescriptor): java.util.stream.Stream[Match] = {
            java.util.stream.Stream.of[Match](Match.exact(td))
          }
        }



      def addToParentFunction(context: SelectorResolver.Context) =
        new java.util.function.Function[Class[_], java.util.stream.Stream[Match]]() {
          def apply(aClass: Class[_]): java.util.stream.Stream[Match] = {
            context.addToParent(classDescriptorFunction(aClass))
              .map[java.util.stream.Stream[Match]](toMatch)
              .orElse(java.util.stream.Stream.empty())
          }
        }

      val classSelectorResolver = new SelectorResolver {

        override def resolve(selector: ClasspathRootSelector, context: SelectorResolver.Context): SelectorResolver.Resolution = {
          val matches =
            ReflectionSupport.findAllClassesInClasspathRoot(selector.getClasspathRoot, isSuitePredicate, alwaysTruePredicate)
              .stream()
              .flatMap(addToParentFunction(context))
              .collect(Collectors.toSet())
          if (matches.isEmpty) {
            Resolution.unresolved()
          } else {
            Resolution.matches(matches)
          }
        }

        override def resolve(selector: PackageSelector, context: SelectorResolver.Context): SelectorResolver.Resolution = {
          val matches =
            ReflectionSupport.findAllClassesInPackage(selector.getPackageName, isSuitePredicate, alwaysTruePredicate)
              .stream()
              .flatMap(addToParentFunction(context))
              .collect(Collectors.toSet())
          if (matches.isEmpty) {
            Resolution.unresolved()
          } else {
            Resolution.matches(matches)
          }
        }

        override def resolve(selector: ModuleSelector, context: SelectorResolver.Context): SelectorResolver.Resolution = {
          val matches =
            ReflectionSupport.findAllClassesInModule(selector.getModuleName, isSuitePredicate, alwaysTruePredicate)
              .stream()
              .flatMap(addToParentFunction(context))
              .collect(Collectors.toSet())
          if (matches.isEmpty) {
            Resolution.unresolved()
          } else {
            Resolution.matches(matches)
          }
        }

        override def resolve(selector: ClassSelector, context: SelectorResolver.Context): SelectorResolver.Resolution = {
          val testClass = selector.getJavaClass
          if (isSuitePredicate.test(testClass)) {
            context.addToParent(
              new java.util.function.Function[TestDescriptor, Optional[ScalaTestClassDescriptor]]() {
                def apply(parent: TestDescriptor): Optional[ScalaTestClassDescriptor] = {
                  val suiteUniqueId = parent.getUniqueId.append(ScalaTestClassDescriptor.segmentType, testClass.getName)
                  parent.getChildren.asScala.find(_.getUniqueId == suiteUniqueId) match {
                    case Some(_) => Optional.empty[ScalaTestClassDescriptor]()
                    case None => Optional.of(new ScalaTestClassDescriptor(engineDesc, suiteUniqueId, testClass, true))
                  }
                }
              })
            .map[Resolution](
              new java.util.function.Function[TestDescriptor, Resolution]() {
                def apply(td: TestDescriptor): Resolution = Resolution.`match`(Match.exact(td))
              }
            ).orElse(Resolution.unresolved())
          }
          else
            Resolution.unresolved()
        }
      }

      val uniqueIdSelectorResolver = new SelectorResolver {
        override def resolve(selector: UniqueIdSelector, context: SelectorResolver.Context): SelectorResolver.Resolution = {
          selector.getUniqueId.getSegments.asScala.toList match {
            case engineSeg :: suiteSeg :: testSeg :: Nil if engineSeg.getType == "engine" && engineSeg.getValue == "scalatest" && testSeg.getType == "test" && suiteSeg.getType == ScalaTestClassDescriptor.segmentType =>
              val suiteClassName = suiteSeg.getValue
              val suiteClass = Class.forName(suiteClassName)
              if (classOf[org.scalatest.Suite].isAssignableFrom(suiteClass)) {
                context.addToParent(
                  new java.util.function.Function[TestDescriptor, Optional[ScalaTestClassDescriptor]]() {
                    def apply(parent: TestDescriptor): Optional[ScalaTestClassDescriptor] = {
                      val children = parent.getChildren.asScala
                      val suiteUniqueId = uniqueId.append(ScalaTestClassDescriptor.segmentType, suiteClass.getName)
                      val testUniqueId = suiteUniqueId.append("test", testSeg.getValue)
                      val testDesc = new ScalaTestDescriptor(testUniqueId, testSeg.getValue, None)
                      val (suiteDesc, result) =
                        children.find(_.getUniqueId == suiteUniqueId) match {
                          case Some(suiteDesc) =>
                            (suiteDesc, Optional.empty[ScalaTestClassDescriptor]())

                          case None =>
                            val suiteDesc = new ScalaTestClassDescriptor(engineDesc, suiteUniqueId, suiteClass, false)
                            (suiteDesc, Optional.of(suiteDesc))
                        }

                      suiteDesc.getChildren.asScala.find(_.getUniqueId == testUniqueId) match {
                        case Some(_) => // Do nothing if the test already exists
                        case None => suiteDesc.addChild(testDesc)
                      }

                      result
                    }
                  }
                )
                .map[Resolution](
                  new java.util.function.Function[TestDescriptor, Resolution]() {
                    def apply(td: TestDescriptor): Resolution = Resolution.`match`(Match.exact(td))
                  }
                )
                .orElse(Resolution.unresolved())
              }
              else
                Resolution.unresolved()

            case engineSeg :: suiteSeg :: Nil if engineSeg.getType == "engine" && engineSeg.getValue == "scalatest" && suiteSeg.getType == ScalaTestClassDescriptor.segmentType =>
              val suiteClassName = suiteSeg.getValue
              val suiteClass = Class.forName(suiteClassName)
              if (classOf[org.scalatest.Suite].isAssignableFrom(suiteClass)) {
                context.addToParent(
                  new java.util.function.Function[TestDescriptor, Optional[ScalaTestClassDescriptor]]() {
                    def apply(parent: TestDescriptor): Optional[ScalaTestClassDescriptor] = {
                      val children = parent.getChildren.asScala
                      val suiteUniqueId = uniqueId.append(ScalaTestClassDescriptor.segmentType, suiteClass.getName)
                      children.find(_.getUniqueId == suiteUniqueId) match {
                        case Some(_) => Optional.empty[ScalaTestClassDescriptor]()
                        case None => Optional.of(new ScalaTestClassDescriptor(engineDesc, suiteUniqueId, suiteClass, false))
                      }
                    }
                  }
                )
                .map[Resolution](
                  new java.util.function.Function[TestDescriptor, Resolution]() {
                    def apply(td: TestDescriptor): Resolution = Resolution.`match`(Match.exact(td))
                  }
                )
                .orElse(Resolution.unresolved())
              }
              else
                Resolution.unresolved()

            case _ => Resolution.unresolved()
          }
        }
      }

      val resolver = EngineDiscoveryRequestResolver.builder[EngineDescriptor]()
                     .addClassContainerSelectorResolver(isSuitePredicate)
                     .addSelectorResolver(classSelectorResolver)
                     .addSelectorResolver(uniqueIdSelectorResolver)
                     .build()

      resolver.resolve(discoveryRequest, engineDesc)

      logger.config("Completed test discovery, discovered suite count: " + engineDesc.getChildren.size())
    }

    engineDesc
  }

  /**
   * Execute ScalaTest suites, you can disable the ScalaTest suites execution by setting system property org.scalatestplus.junit.JUnit5TestEngine.disabled to "true".
   */
  def execute(request: ExecutionRequest): Unit = {
    if (System.getProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled") != "true") {
      logger.fine("Start tests execution...")
      val engineDesc = request.getRootTestDescriptor
      val listener = request.getEngineExecutionListener

      listener.executionStarted(engineDesc)
      var engineFailure: Option[Throwable] = None
      try {
        val numThreadsProp = System.getProperty("org.scalatestplus.junit5.numThreads", "1")
        val numThreads =
          Try(numThreadsProp.toInt).getOrElse(
            throw new RuntimeException(Resources.invalidNumThreads(numThreadsProp))
          )
        if (numThreads < 0)
          throw new RuntimeException(Resources.invalidNumThreads(numThreadsProp))

        val suiteDescriptors = engineDesc.getChildren.asScala.flatMap {
          case clzDesc: ScalaTestClassDescriptor => Some(clzDesc)
          case otherDesc =>
            logger.warning("Found test descriptor " + otherDesc.toString + " that is not supported, skipping.")
            None
        }.toList

        if (numThreads == 1) {
          // ParallelTestExecution falls back to serial execution without a distributor.
          suiteDescriptors.foreach { clzDesc =>
            val completion = new CompletableFuture[Option[Throwable]]()
            runSuite(clzDesc, listener, engineDesc, None, completion)
            awaitCompletion(completion).foreach { t =>
              if (engineFailure.isEmpty) engineFailure = Some(t)
            }
          }
        } else {
          val poolSize = if (numThreads == 0) Runtime.getRuntime.availableProcessors * 2 else numThreads
          val suiteExecSvc = createExecutor(poolSize, "ScalaTest-Suite")
          val distributor = new ConcurrentDistributor(suiteExecSvc)
          val completions = ListBuffer.empty[CompletableFuture[Option[Throwable]]]
          try {
            suiteDescriptors.foreach { clzDesc =>
              val completion = new CompletableFuture[Option[Throwable]]()
              completions += completion
              try {
                suiteExecSvc.submit(new Runnable {
                  override def run(): Unit =
                    runSuite(clzDesc, listener, engineDesc, Some(distributor), completion)
                })
              }
              catch {
                case t: Throwable =>
                  completion.complete(Some(t))
                  throw t
              }
            }
            val errors = completions.toList.flatMap(awaitCompletion)
            errors.headOption.foreach { t => engineFailure = Some(t) }
          } finally {
            // Drain submitted suites even if submitting a later one failed.
            completions.foreach(awaitCompletion)
            shutdownAndAwait(suiteExecSvc)
          }
        }

        logger.fine("Completed tests execution.")
      } catch {
        case t: Throwable =>
          if (engineFailure.isEmpty) engineFailure = Some(t)
          throw t
      } finally {
        listener.executionFinished(
          engineDesc,
          engineFailure.fold(TestExecutionResult.successful())(TestExecutionResult.failed)
        )
      }
    }
  }

  private def createExecutor(poolSize: Int, threadPrefix: String): ExecutorService = {
    val threadCounter = new AtomicInteger
    val threadFactory = new ThreadFactory {
      val defaultThreadFactory = Executors.defaultThreadFactory
      def newThread(runnable: Runnable): Thread = {
        val thread = defaultThreadFactory.newThread(runnable)
        thread.setName(threadPrefix + "-" + threadCounter.incrementAndGet())
        thread
      }
    }
    if (poolSize > 0) Executors.newFixedThreadPool(poolSize, threadFactory)
    else Executors.newCachedThreadPool(threadFactory)
  }

  private def shutdownAndAwait(execSvc: ExecutorService): Unit = {
    execSvc.shutdown()
    // A completed status need not mean all queued distributor work has stopped reporting.
    if (!execSvc.awaitTermination(1, TimeUnit.HOURS))
      execSvc.shutdownNow()
  }

  private def awaitCompletion(completion: CompletableFuture[Option[Throwable]]): Option[Throwable] = {
    var interrupted = false
    var completed = false
    var result: Option[Throwable] = None
    while (!completed) {
      try {
        result = completion.get()
        completed = true
      }
      catch {
        case _: InterruptedException =>
          interrupted = true
      }
    }
    if (interrupted)
      Thread.currentThread().interrupt()
    result
  }

  /**
   * Complete the future when the suite's Status finishes, without waiting for
   * distributed work on an executor worker.
   */
  private def runSuite(
    clzDesc: ScalaTestClassDescriptor,
    listener: org.junit.platform.engine.EngineExecutionListener,
    engineDesc: org.junit.platform.engine.TestDescriptor,
    distributor: Option[ConcurrentDistributor],
    completion: CompletableFuture[Option[Throwable]]
  ): Unit = {
    logger.fine("Start execution of suite class " + clzDesc.suiteClass.getName + "...")
    var reporter: EngineExecutionListenerReporter = null
    val finishStarted = new java.util.concurrent.atomic.AtomicBoolean(false)

    def finishSuite(failure: Option[Throwable]): Unit = {
      if (finishStarted.compareAndSet(false, true)) {
        try {
          // SuiteAborted may already have finished clzDesc.
          if (reporter == null || reporter.suiteResult.isEmpty) {
            val result = failure.fold(TestExecutionResult.successful())(TestExecutionResult.failed)
            if (reporter == null)
              listener.executionFinished(clzDesc, result)
            else
              reporter.finishSuite(result)
          }
        }
        finally {
          val reported = failure.orElse(if (reporter == null) None else reporter.runAborted)
          completion.complete(reported)
        }
      }
    }

    try {
      listener.executionStarted(clzDesc)
      val suiteClass = clzDesc.suiteClass
      val canInstantiate = JUnitHelper.checkForPublicNoArgConstructor(suiteClass) && classOf[org.scalatest.Suite].isAssignableFrom(suiteClass)
      require(canInstantiate, "Must pass an org.scalatest.Suite with a public no-arg constructor")
      val suiteToRun = suiteClass.newInstance.asInstanceOf[org.scalatest.Suite]
      reporter = new EngineExecutionListenerReporter(listener, clzDesc, engineDesc)
      val children = clzDesc.getChildren.asScala

      val filter =
        // When JUnit filters out every discovered test, do not run them again.
        if (children.isEmpty && clzDesc.autoAddTestChildren && suiteToRun.testNames.nonEmpty)
          Filter(
            tagsToInclude = Some(Set("Selected")),
            excludeNestedSuites = true,
            dynaTags = DynaTags(Map.empty, Map(suiteToRun.suiteId -> Map.empty))
          )
        else if (children.isEmpty)
          Filter(
            tagsToInclude = None,
            excludeNestedSuites = false,
            dynaTags = DynaTags(Map.empty, Map(suiteToRun.suiteId -> Map.empty))
          )
        else if (suiteToRun.testNames.size == children.size) // When testNames size is same as children size, it means all tests are selected, so no need to apply filter, this solves the issue of dynamic test names when running suite.
          Filter.default
        else {
          val SelectedTag = "Selected"
          val SelectedSet = Set(SelectedTag)
          val testNames = suiteToRun.testNames
          val desiredTests: Set[String] =
            children.map(_.getDisplayName).filter { tn =>
              testNames.contains(tn) || testNames.contains(NameTransformer.decode(tn))
            }.toSet
          val taggedTests: Map[String, Set[String]] = desiredTests.map(_ -> SelectedSet).toMap
          val suiteId = suiteToRun.suiteId
          Filter(
            tagsToInclude = Some(SelectedSet),
            excludeNestedSuites = true,
            dynaTags = DynaTags(Map.empty, Map(suiteId -> taggedTests))
          )
        }

      val status = suiteToRun.run(None, Args(reporter, Stopper.default, filter, ConfigMap.empty, distributor, new Tracker))
      status.whenCompleted {
        case Success(_) =>
          // Test failures belong to test descriptors, not the suite descriptor.
          finishSuite(None)
        case Failure(t) =>
          finishSuite(Some(t))
      }
      logger.config("Started execution of suite class " + clzDesc.suiteClass.getName + ".")
    } catch {
      case t: Throwable =>
        finishSuite(Some(t))
    }
  }
}
