package org.scalatestplus.junit5

import org.junit.platform.engine.{ConfigurationParameters, EngineExecutionListener, ExecutionRequest, TestDescriptor, TestExecutionResult}
import org.junit.platform.engine.UniqueId
import org.junit.platform.engine.discovery.ClasspathRootSelector
import org.junit.platform.engine.discovery.DiscoverySelectors.{selectClass, selectClasspathRoots, selectPackage}
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder.request
import org.scalatest.{BeforeAndAfterAll, funspec}
import org.scalatestplus.junit5.helpers.{FailingScalaTestSuite, HappySuite, ParallelHelperSuite1, ParallelHelperSuite2, ParallelSuiteHelper, ParallelTestExecutionThreadCaptureSuite, ParallelTestExecutionSuite1, ParallelTestExecutionSuite2, RunAbortingSuite, SimpleScalaTestSuite, SuiteAbortingSuite, TaggedAnyFunSuite, TaggedAnyFunSuiteState}

import java.nio.file.{Files, Paths}
import java.util.Optional
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConverters._

class ScalaTestEngineSpec extends funspec.AnyFunSpec with BeforeAndAfterAll {
  val engine = new ScalaTestEngine
  var scalaTestEngineProperty: Option[String] = None

  private val emptyConfig: ConfigurationParameters = new ConfigurationParameters {
    def get(key: String): Optional[String] = Optional.empty()
    def getBoolean(key: String): Optional[java.lang.Boolean] = Optional.empty()
    def size(): Int = 0
    def keySet(): java.util.Set[String] = java.util.Collections.emptySet()
  }

  private def capturingListener(results: CopyOnWriteArrayList[TestExecutionResult]): EngineExecutionListener =
    capturingListener(results, None)

  private def capturingListener(
    results: CopyOnWriteArrayList[TestExecutionResult],
    events: Option[CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]]
  ): EngineExecutionListener =
    new EngineExecutionListener {
      override def executionStarted(td: TestDescriptor): Unit = ()
      override def executionFinished(td: TestDescriptor, result: TestExecutionResult): Unit = {
        results.add(result)
        events.foreach(_.add((td, result)))
      }
      override def executionSkipped(td: TestDescriptor, reason: String): Unit = ()
    }

  private def assertParallelTestExecutionRunsOnEngineThread(): Unit = {
    ParallelSuiteHelper.reset()
    val discoveryRequest = request.selectors(selectClass(classOf[ParallelTestExecutionThreadCaptureSuite])).build()
    val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
    val results = new CopyOnWriteArrayList[TestExecutionResult]()
    val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
    val engineThread = Thread.currentThread()
    engine.execute(execRequest)

    assert(ParallelSuiteHelper.testThread1 eq engineThread, "test 1 was distributed to another thread")
    assert(ParallelSuiteHelper.testThread2 eq engineThread, "test 2 was distributed to another thread")
    assert(results.size() == 4, s"Expected two tests, suite, and engine to finish, got: $results")
    assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
      s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
  }

  override def beforeAll(): Unit = {
    scalaTestEngineProperty = Option(System.clearProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled"))
  }

  override def afterAll(): Unit = {
    scalaTestEngineProperty.foreach(System.setProperty("org.scalatestplus.junit5.ScalaTestEngine.disabled", _))
  }

  describe("ScalaTestEngine") {
    describe("discover method") {
      it("should discover suites on classpath") {
        val classPathRoot = classOf[ScalaTestEngineSpec].getProtectionDomain.getCodeSource.getLocation
        val discoveryRequest = request.selectors(
          selectClasspathRoots(java.util.Collections.singleton(Paths.get(classPathRoot.toURI)))
        ).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.exists(td => td.asInstanceOf[ScalaTestClassDescriptor].suiteClass == classOf[HappySuite]))
      }

      it("should return unresolved for classpath without any tests") {
        val emptyPath = Files.createTempDirectory(null)
        val discoveryRequest = request.selectors(
          selectClasspathRoots(java.util.Collections.singleton(emptyPath))
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.isEmpty)
      }

      it("should discover suites in package") {
        val discoveryRequest = request.selectors(
          selectPackage("org.scalatestplus.junit5.helpers")
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.exists(td => td.asInstanceOf[ScalaTestClassDescriptor].suiteClass == classOf[HappySuite]))
      }

      it("should return unresolved for package without any tests") {
        val discoveryRequest = request.selectors(
          selectPackage("org.scalatestplus.junit5.nonexistant")
        ).build()

        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        assert(engineDescriptor.getChildren.asScala.isEmpty)
      }
    }

    describe("execute method") {
      it("should run suites sequentially by default (numThreads=1)") {
        val discoveryRequest = request.selectors(selectClass(classOf[SimpleScalaTestSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
        engine.execute(execRequest)

        assert(results.size() == 3, s"Expected 3 results (test+suite+engine) but got: $results")
        assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
          s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
      }

      it("should continue running remaining suites in sequential mode when one fails") {
        val discoveryRequest = request.selectors(
          selectClass(classOf[FailingScalaTestSuite]),
          selectClass(classOf[SimpleScalaTestSuite])
        ).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
        engine.execute(execRequest)

        val statuses = results.asScala.map(_.getStatus).toList
        assert(statuses.exists(_ == TestExecutionResult.Status.FAILED),
          s"Expected at least one FAILED result but got: $statuses")
        assert(statuses.exists(_ == TestExecutionResult.Status.SUCCESSFUL),
          s"Expected at least one SUCCESSFUL result but got: $statuses")
      }

      it("should report SuiteAborted on the class descriptor without failing the engine descriptor") {
        val discoveryRequest = request.selectors(selectClass(classOf[SuiteAbortingSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        val classDescriptor = engineDescriptor.getChildren.asScala.head

        val results = new CopyOnWriteArrayList[TestExecutionResult]()
        val events = new CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]()
        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results, Some(events)), emptyConfig)
        engine.execute(execRequest)

        val classFinishes = events.asScala.filter(_._1 == classDescriptor)
        assert(classFinishes.size == 1, s"Expected one class finish but got: $classFinishes")
        assert(classFinishes.head._2.getStatus == TestExecutionResult.Status.ABORTED,
          s"SuiteAborted must not be overwritten, got: ${classFinishes.head._2.getStatus}")
        val engineFinishes = events.asScala.filter(_._1 == engineDescriptor)
        assert(engineFinishes.size == 1, s"Expected one engine finish but got: $engineFinishes")
        assert(engineFinishes.head._2.getStatus == TestExecutionResult.Status.SUCCESSFUL,
          s"A suite abort must not fail the engine descriptor, got: ${engineFinishes.head._2.getStatus}")
      }

      it("should not aggregate a suite abort into the engine result in parallel mode") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "2")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[SuiteAbortingSuite]),
            selectClass(classOf[SimpleScalaTestSuite])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
          val classDescriptors = engineDescriptor.getChildren.asScala.collect {
            case descriptor: ScalaTestClassDescriptor => descriptor.suiteClass.getName -> descriptor
          }.toMap

          val events = new CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]()
          val execRequest = ExecutionRequest.create(
            engineDescriptor,
            capturingListener(new CopyOnWriteArrayList[TestExecutionResult](), Some(events)),
            emptyConfig
          )
          engine.execute(execRequest)

          def statuses(descriptor: TestDescriptor): List[TestExecutionResult.Status] =
            events.asScala.collect { case (`descriptor`, result) => result.getStatus }.toList

          assert(statuses(classDescriptors(classOf[SuiteAbortingSuite].getName)) == List(TestExecutionResult.Status.ABORTED))
          assert(statuses(classDescriptors(classOf[SimpleScalaTestSuite].getName)) == List(TestExecutionResult.Status.SUCCESSFUL))
          assert(statuses(engineDescriptor) == List(TestExecutionResult.Status.SUCCESSFUL))
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should fail the engine descriptor on RunAborted without finishing it twice") {
        val discoveryRequest = request.selectors(selectClass(classOf[RunAbortingSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

        val events = new CopyOnWriteArrayList[(TestDescriptor, TestExecutionResult)]()
        val execRequest = ExecutionRequest.create(
          engineDescriptor,
          capturingListener(new CopyOnWriteArrayList[TestExecutionResult](), Some(events)),
          emptyConfig
        )
        engine.execute(execRequest)

        val engineFinishes = events.asScala.filter(_._1 == engineDescriptor)
        assert(engineFinishes.size == 1, s"RunAborted must not double-finish the engine, got: $engineFinishes")
        assert(engineFinishes.head._2.getStatus == TestExecutionResult.Status.FAILED,
          s"RunAborted must fail the engine descriptor, got: ${engineFinishes.head._2.getStatus}")
      }

      it("should run ParallelTestExecution tests serially when numThreads is unset") {
        System.clearProperty("org.scalatestplus.junit5.numThreads")
        assertParallelTestExecutionRunsOnEngineThread()
      }

      it("should run ParallelTestExecution tests serially when numThreads=1") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "1")
        try {
          assertParallelTestExecutionRunsOnEngineThread()
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should not run a suite when all of its discovered tests were filtered out") {
        TaggedAnyFunSuiteState.testRan = false
        val discoveryRequest = request.selectors(selectClass(classOf[TaggedAnyFunSuite])).build()
        val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
        val classDescriptor = engineDescriptor.getChildren.asScala.collectFirst {
          case descriptor: ScalaTestClassDescriptor => descriptor
        }.get

        // Simulate JUnit Platform's post-discovery excludeTags filter removing
        // the suite's only tagged test while retaining the untagged container.
        classDescriptor.getChildren.asScala.foreach(classDescriptor.removeChild)

        val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(new CopyOnWriteArrayList[TestExecutionResult]()), emptyConfig)
        engine.execute(execRequest)

        assert(!TaggedAnyFunSuiteState.testRan,
          "A test removed by post-discovery filtering was executed again")
      }

      it("should run suites in parallel when numThreads=2") {
        // The barrier times out if the suites run sequentially.
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "2")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelHelperSuite1]),
            selectClass(classOf[ParallelHelperSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          assert(ParallelSuiteHelper.thread1Name != null, "ParallelHelperSuite1 did not record a thread name")
          assert(ParallelSuiteHelper.thread2Name != null, "ParallelHelperSuite2 did not record a thread name")
          assert(ParallelSuiteHelper.thread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread1Name}")
          assert(ParallelSuiteHelper.thread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread2Name}")
          assert(ParallelSuiteHelper.thread1Name != ParallelSuiteHelper.thread2Name,
            "Both suites ran on the same thread – they were not truly parallel")
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should run suites in parallel when numThreads=0 (auto-detect)") {
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "0")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelHelperSuite1]),
            selectClass(classOf[ParallelHelperSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          assert(ParallelSuiteHelper.thread1Name != null, "ParallelHelperSuite1 did not record a thread name")
          assert(ParallelSuiteHelper.thread2Name != null, "ParallelHelperSuite2 did not record a thread name")
          assert(ParallelSuiteHelper.thread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread1Name}")
          assert(ParallelSuiteHelper.thread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected suite-pool thread name but got: ${ParallelSuiteHelper.thread2Name}")
          assert(ParallelSuiteHelper.thread1Name != ParallelSuiteHelper.thread2Name,
            "Both suites ran on the same thread – they were not truly parallel")
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should share the suite pool with ParallelTestExecution tests") {
        ParallelSuiteHelper.reset()
        System.setProperty("org.scalatestplus.junit5.numThreads", "2")
        try {
          val discoveryRequest = request.selectors(
            selectClass(classOf[ParallelTestExecutionSuite1]),
            selectClass(classOf[ParallelTestExecutionSuite2])
          ).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))

          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          engine.execute(execRequest)

          assert(ParallelSuiteHelper.testThread1Name != null,
            "ParallelTestExecutionSuite1 did not record a test thread name")
          assert(ParallelSuiteHelper.testThread2Name != null,
            "ParallelTestExecutionSuite2 did not record a test thread name")
          assert(ParallelSuiteHelper.testThread1Name.startsWith("ScalaTest-Suite-"),
            s"Expected shared suite-pool thread name but got: ${ParallelSuiteHelper.testThread1Name}")
          assert(ParallelSuiteHelper.testThread2Name.startsWith("ScalaTest-Suite-"),
            s"Expected shared suite-pool thread name but got: ${ParallelSuiteHelper.testThread2Name}")
          assert(ParallelSuiteHelper.testThread1Name != ParallelSuiteHelper.testThread2Name,
            "Distributed tests did not run concurrently on distinct shared-pool threads")
          assert(results.asScala.forall(_.getStatus == TestExecutionResult.Status.SUCCESSFUL),
            s"Unexpected failures: ${results.asScala.filter(_.getStatus != TestExecutionResult.Status.SUCCESSFUL)}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should throw RuntimeException for invalid numThreads value") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "notANumber")
        try {
          val discoveryRequest = request.selectors(selectClass(classOf[HappySuite])).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          intercept[RuntimeException] {
            engine.execute(execRequest)
          }
          assert(results.size() == 1, s"Expected executionFinished to be called once but got: ${results.size()}")
          assert(results.get(0).getStatus == TestExecutionResult.Status.FAILED,
            s"Expected FAILED engine result but got: ${results.get(0).getStatus}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }

      it("should throw RuntimeException for negative numThreads value") {
        System.setProperty("org.scalatestplus.junit5.numThreads", "-1")
        try {
          val discoveryRequest = request.selectors(selectClass(classOf[HappySuite])).build()
          val engineDescriptor = engine.discover(discoveryRequest, UniqueId.forEngine(engine.getId()))
          val results = new CopyOnWriteArrayList[TestExecutionResult]()
          val execRequest = ExecutionRequest.create(engineDescriptor, capturingListener(results), emptyConfig)
          intercept[RuntimeException] {
            engine.execute(execRequest)
          }
          assert(results.size() == 1, s"Expected executionFinished to be called once but got: ${results.size()}")
          assert(results.get(0).getStatus == TestExecutionResult.Status.FAILED,
            s"Expected FAILED engine result but got: ${results.get(0).getStatus}")
        } finally {
          System.clearProperty("org.scalatestplus.junit5.numThreads")
        }
      }
    }
  }
}
