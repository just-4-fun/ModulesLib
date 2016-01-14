package test

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}
import just4fun.core.async.{FutureContext, DefaultFutureContext, FutureX}
import just4fun.core.modules._
import just4fun.utils.logger.Logger._


/* CONFIG */
case class TestConfig(var startRestful: Boolean = false, var startSuspended: Boolean = false, var bits: Int = 0, var activatingDelay: Int = 1000, var deactivatingDelay: Int = 1000, var restLatency: Int = 1000, var destroyLatency: Int = 1000, var activOpt: Int = 2, var deactOpt: Int = 2) {
	val injects = ArrayBuffer[Any ⇒ Unit]().padTo(HitPoints.modPoints.size, null)
	def hasInject(ix: Int): Boolean = (bits & (1 << ix)) != 0 && injects(ix) != null
	def execInject(ix: Int, param: Any): Unit = if ((bits & (1 << ix)) != 0 && injects(ix) != null) injects(ix)(param)
	def switchInject(ix: Int, isOn: Boolean): Unit = if (isOn) bits |= (1 << ix) else bits &= ~(1 << ix)
	def setInject(p: HitPoint, isOn: Boolean, code: Any ⇒ Unit): TestConfig = {
		val ix = p.index
		injects(ix) = code
		if (isOn) bits |= (1 << ix) else bits &= ~(1 << ix)
		this
	}
	def printInjects(): Unit = {
		logV(HitPoints.modPoints.map(p ⇒ s"${p.id}:$p=${if (hasInject(p.id)) "1" else "0"}").mkString(" : "))
	}
}


/* SYSTEM */
class TestSystem extends ModuleSystem {
	import HitPoints._
	import TestApp._
	protected[this] implicit val thisSys: this.type = this
	override implicit val asyncContext: DefaultFutureContext = new DefaultFutureContext
	var prepareDelay = 0
	val app = TestApp()
	asyncContext.start()

	def await() = asyncContext.await()
	/* callbacks */
	override protected[this] def onSystemStart(): Unit = {
		SysStart.hit()(null)
		logV(s"@ System  onSystemStart", tagCallbacks)
	}
	override protected[this] def onSystemStop(): Unit = {
		SysFinish.hit()(null)
		logV(s"@ System  onSystemFinish", tagCallbacks)
		TestApp().onSystemFinish()
	}
	override protected[this] def onModulePrepare(promise: ModulePreparePromise): Unit = {
		SysModPrepare.hit()(null)
		logV(s"@ System  onModulePrepare [${promise.module.getClass.getSimpleName}]", tagCallbacks)
		if (prepareDelay == 0) promise.complete()
		else FutureX.post(prepareDelay, promise.module)(promise.complete())
	}
	override protected[this] def onModuleDestroy(m: Module): Unit = {
		SysModDestroy.hit()(null)
		logV(s"@ System  onModuleDestroy [${m.getClass.getSimpleName}]", tagCallbacks)
	}
}


/* MODULE */
abstract class TestModule(val id: Int = 0) extends Module {
	import HitPoints._
	import TestApp._
	type System <: TestSystem
	val moduleId = getClass.getSimpleName
	val app = TestApp()
	val config: TestConfig = app.setModule(this)
	import config._
	private[this] var activatingDone = false
	private[this] var deactivatingDone = false
	protected[this] var count = 0
	override protected[this] val internal = new TestCallbacks
	override protected[this] val lifeCycle = new TestLifeCycle
	//
	ModCreate.hit()
	if (startRestful) setRestful(true)
	if (startSuspended) suspendService(true)


	override def setRestful(v: Boolean) { super.setRestful(v) }
	override def suspendService(v: Boolean) { super.suspendService(v) }
	override def recover() { super.recover() }
	def setFailed() { setFailed(new TestingException) }
	def bind(clas: Class[_ <: TestModule], sync: Boolean): Unit = internal.internalBind(clas, sync)
	def unbind(clas: Class[_ <: TestModule]): Unit = internal.internalUnbind(clas)
	def use(time: Int = 0): Unit = {
		serveAsync {
			count += 1
			ModReqExec.hit(count)
			logV(s"[$moduleId]  $count  request...", Module.tagState)
			if (time != 0) Thread.sleep(time)
			count
		}.onComplete {
			case Failure(e) ⇒ logV(s"[$moduleId] $count request failed with $e", Module.tagState)
				ModReqComplete.hit(false)
			case Success(n) ⇒ logV(s"[$moduleId]  $n  request ok", Module.tagState)
				ModReqComplete.hit(true)
		}
	}

	/* callbacks */
	class TestCallbacks extends InternalCallbacks {
		override protected[this] def onConstructed(): Unit = {
			ModConstr.hit()
			logV(s"@ [$moduleId] onConstructed", tagCallbacks)
		}
		override protected[this] def onPrepared(): Unit = {
			ModPrepare.hit()
			logV(s"@ [$moduleId] onPrepared", tagCallbacks)
		}
		override protected[this] def onDestroyed(): Unit = {
			ModDestroy.hit()
			logV(s"@ [$moduleId] onDestroyed", tagCallbacks)
		}
	}

	/**/
	class TestLifeCycle extends LifeCycle {
		restLatencyMs = restLatency
		destroyLatencyMs = destroyLatency
		// STATE callbacks
		override protected[this] def onFailure(exception: Throwable): Option[Throwable] = {
			ModFailure.hit(exception)
			logV(s"@ [$moduleId] onFailure $exception", tagCallbacks)
			Some(exception)
		}
		override protected[this] def onFailed(): Unit = {
			ModFailed.hit()
			logV(s"@ [$moduleId] onFailed", tagCallbacks)
		}
		override protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = {
			ModActStart.hit(creating)
			logV(s"@ [$moduleId] onActivatingStart;  creating? $creating", tagCallbacks)
			FutureX.post(activatingDelay, this) {
				activatingDone = true
				if (config.activOpt  == 1) completeActivating()
			}
			config.activOpt match {
				case 0 ⇒CompleteNow
				case 1 ⇒CompleteManually
				case 2 ⇒CompleteSelector.when {
					ModActProgress.hit(activatingDone)
					logV(s"@ [$moduleId]:  Activating...", tagCallbacks)
					activatingDone
				}
			}
		}
		override protected[this] def onActivatingComplete(creating: Boolean): Unit = {
			ModActCompl.hit(creating)
			activatingDone = false
			logV(s"@ [$moduleId] onActivatingComplete;  creating? $creating", tagCallbacks)
		}
		override protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = {
			ModDeactStart.hit(destroying)
			logV(s"@ [$moduleId] onDeactivatingStart;  destroying? $destroying", tagCallbacks)
			FutureX.post(deactivatingDelay, this) {
				deactivatingDone = true
				if (config.deactOpt  == 1) completeDeactivating()
			}
			config.deactOpt match {
				case 0 ⇒CompleteNow
				case 1 ⇒CompleteManually
				case 2 ⇒CompleteSelector.when {
					ModDeactProgress.hit(deactivatingDone)
					logV(s"@ [$moduleId]:  Deactivating...", tagCallbacks)
					deactivatingDone
				}
			}
		}
		override protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = {
			ModDeactCompl.hit(destroying)
			deactivatingDone = false
			logV(s"@ [$moduleId] onDeactivatingComplete;  destroying? $destroying", tagCallbacks)
		}
		// BINDING callbacks
		override protected[this] def onBindingAdd(module: Module, sync: Boolean): Unit = {
			ModBindAdd.hit(module)
			logV(s"@ [$moduleId] onBound [${module.getClass.getSimpleName}];  sync? $sync", tagCallbacks)
		}
		override protected[this] def onBindingRemove(module: Module): Unit = {
			ModBindRemove.hit(module)
			logV(s"@ [$moduleId] onUnbound [${module.getClass.getSimpleName}];  unbound? $isUnbound", tagCallbacks)
		}
		// REQUEST callbacks
		override protected[this] def onRequestAdding(request: ModuleRequest[_]): Unit = {
			ModReqAdd.hit(request)
			logV(s"@ [$moduleId] onRequestAdd;  hasRequests? $hasRequests", tagCallbacks)
		}
		override protected[this] def onRequestRemoved(request: ModuleRequest[_]): Unit = {
			ModReqRemove.hit(request)
			logV(s"@ [$moduleId] onRequestRemove;  hasRequests? $hasRequests", tagCallbacks)
		}
		override protected[this] def progressBackoff: Long = {
			val durationMs = progressDurationMs
			val delay = if (durationMs < 2000) 500
			else if (durationMs < 10000) 2000
			else if (durationMs < 60000) 10000
			else 10000
			if (isActivating) delay else (delay * 2).toLong
		}
	}
}




/* CONNECTOR */
abstract class TestConnector[M <: TestModule](implicit val context: FutureContext) extends ModuleConnector[M] {
	def start(): Unit = module
	def stop(): Unit = moduleDisconnect()
}




class Module1 extends TestModule(1)
class Module2 extends TestModule(2)
class Module3 extends TestModule(3)
class Module4 extends TestModule(4)
class Module5 extends TestModule(5)




class TestingException extends Exception