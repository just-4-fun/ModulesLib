package just4fun.core.modules

import just4fun.core.async.FutureX.DummyImplicit2
import just4fun.core.async.{FutureContextOwner, FutureContext, FutureX}
import just4fun.utils.logger.Logger._
import just4fun.utils.logger.LoggerConfig

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.{Failure, Try}


/* STATES */
object ModuleState extends Enumeration {
	type ModuleState = Value
	val PREPARING, RESTING, ACTIVATING, OPERATING, DEACTIVATING, FAILED, DESTROYED = Value
}


object Module {
	private[modules] val uFlagNONE = 0
	private[modules] val uFlagREPEAT = 1
	private[modules] val uFlagBUSY = 2
	private[modules] val uFlagTOUCHED = 3
	private[modules] val uFlagRESUME = 4
	val tagState = 0x7102111
	val tagStateX = 0x7102112
	val tagParam = 0x7102113
}

/* MODULE */
trait Module extends StateProcessor with BindingProcessor with RequestProcessor with FutureContextOwner {
	import ModuleState._
	type System <: ModuleSystem
	implicit final val thisModule: this.type = this
	protected[this] final val system: System = ModuleSystem.instance.asInstanceOf[System]
	implicit protected val asyncContext: FutureContext = system.asyncContext
	protected[this] final val restored = system.attach
	protected[this] val restorable = true
	protected[this] val lifeCycle = new LifeCycle
	protected[this] val internal = new InternalCallbacks
	private[this] lazy val stopIDs = mutable.Set[Int]()

	/* INTERNAL */
	private[modules] def isRestorable: Boolean = restorable
	private[modules] def started(id: Int): Unit = stopIDs += id
	private[modules] def stopped(id: Int): Boolean = {
		stopIDs -= id
		stopIDs.isEmpty
	}
	private[modules] def boundTo(client: Module): Boolean = isBoundTo(client)

	/* LIFECYCLE CALLBACKS */
	class LifeCycle {
		import ModuleState._
		import StateParams._
		protected[this] var restLatencyMs = 10000
		protected[this] var destroyLatencyMs = 0
		// STATE callbacks
		final def completeActivating(): Unit = if (isActivating) on_!!!(Complete)
		final def completeDeactivating(): Unit = if (isDeactivating) on_!!!(Complete)
		protected[this] def onFailure(exception: Throwable): Option[Throwable] = Some(exception)
		protected[this] def onFailed(): Unit = ()
		protected[this] def onActivatingStart(creating: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		//		protected[this] def isActivatingComplete(initializing: Boolean, durationMs: Long): Boolean = true
		protected[this] def onActivatingComplete(creating: Boolean): Unit = ()
		protected[this] def onDeactivatingStart(destroying: Boolean, complete: CompleteSelector): CompleteOption = complete.now
		//		protected[this] def isDeactivatingComplete(destroying: Boolean, durationMs: Long): Boolean = true
		protected[this] def onDeactivatingComplete(destroying: Boolean): Unit = ()
		protected[this] final def progressDurationMs: Long = progressDuration
		protected[this] def progressBackoff: Long = calcBackoff
		// BINDING callbacks
		protected[this] def onBindingAdd(module: Module, sync: Boolean): Unit = ()
		protected[this] def onBindingRemove(module: Module): Unit = ()
		// REQUEST callbacks
		protected[this] def onRequestAdding(request: ModuleRequest[_]): Unit = {}
		protected[this] def onRequestRemoved(request: ModuleRequest[_]): Unit = {}
		// BACK DOOR
		private[modules] def callFailed(): Unit = onFailed()
		private[modules] def callFailure(e: Throwable): Option[Throwable] = {
			try onFailure(e) catch {case e: Throwable ⇒ logE(s"onFailure thrown $e"); Some(e)}
		}
		private[modules] def callStart(): CompleteOption = {
			if (isActivating) onActivatingStart(is(Creating), CompleteSelector) else onDeactivatingStart(is(Detached), CompleteSelector)
		}
		private[modules] def callComplete(): Unit = {
			if (isActivating) onActivatingComplete(is(Creating)) else onDeactivatingComplete(is(Detached))
		}
		//		private[modules] def isComplete(durationMs: Long): Boolean = {
		//			if (isActivating) isActivatingComplete(is(Initializing), durationMs) else isDeactivatingComplete(is(Destroying), durationMs)
		//		}
		private[modules] def callBindAdd(module: Module, sync: Boolean): Unit = onBindingAdd(module, sync)
		private[modules] def callBindRemove(module: Module): Unit = onBindingRemove(module)
		private[modules] def callRequestAdding(request: ModuleRequest[_]): Unit = onRequestAdding(request)
		private[modules] def callRequestRemoved(request: ModuleRequest[_]): Unit = onRequestRemoved(request)
		private[modules] def latency = if (isUnbound) destroyLatencyMs else restLatencyMs
		private[modules] def backoff: Long = try progressBackoff catch loggedE(calcBackoff)
		private[this] def calcBackoff: Long = {
			val durationMs = progressDurationMs
			val delay = if (durationMs < 2000) 200
			else if (durationMs < 10000) 1000
			else if (durationMs < 60000) 5000
			else 10000
			if (isActivating) delay else (delay * 1.5).toLong
		}
	}


	/* INTERNAL CALLBACKS */
	class InternalCallbacks {
		protected[this] def onConstructed(): Unit = ()
		protected[this] def onPrepared(): Unit = ()
		protected[this] def onDestroyed(): Unit = ()
		private[modules] def callConstructed() = onConstructed()
		private[modules] def callPrepared() = onPrepared()
		private[modules] def callDestroyed() = onDestroyed()
		def internalBind[M <: Module](clas: Class[M], sync: Boolean): M = system.bind[M](clas, thisModule, sync)
		def internalUnbind[M <: Module](clas: Class[M]): Unit = system.unbind[M](clas, thisModule)
	}

}





/* BINDING PROCESSOR */
trait BindingProcessor {
	self: Module =>
	import StateParams._
	private[this] val clients = mutable.Set[Module]()
	private[modules] val syncClients = mutable.Set[Module]()
	private[modules] val syncServers = mutable.Set[Module]()

	def isBound: Boolean = clients.nonEmpty
	def isUnbound: Boolean = clients.isEmpty
	protected[this] def bind[M <: Module : Manifest]: M = macro Macros.bind[M]
	protected[this] def bindSync[M <: Module : Manifest]: M = macro Macros.bindS[M]
	protected[this] def unbind[M <: Module : Manifest]: Unit = {
		internal.internalUnbind(implicitly[Manifest[M]].runtimeClass.asInstanceOf[Class[M]])
	}
	protected[this] def unbind[M <: Module : Manifest](s: M): Unit = {
		internal.internalUnbind(s.getClass.asInstanceOf[Class[M]])
	}
	protected def isBoundTo(client: Module): Boolean = clients.contains(client)
	/* INTERNAL */
	private[modules] def hasSyncServers: Boolean = syncServers.nonEmpty
	private[modules] def bindingAdd(client: Module, sync: Boolean): Unit = clients += client
	private[modules] def onBindingAdd(client: Module, sync: Boolean): Unit ={
		clients.contains(client) match {
			case true ⇒
				if (lifeCycle != null) lifeCycle.callBindAdd(client, sync)
				if (not(HasBindings)) on_!!!(HasBindings)
				if (sync) addSyncClient(client)
				if (LoggerConfig.isDebug) client.detectCyclicBinding(this, client :: Nil) match {
					case Nil =>
					case trace => logW(s"Cyclic usage detected in chain [${trace.map(_.getClass.getSimpleName).mkString(", ")}]")
				}
			case _ ⇒ if (!sync) removeSyncClient(client)
		}
	}
	private[modules] def bindingRemove(client: Module): Unit = {
		if (clients.remove(client)) {
			if (lifeCycle != null) lifeCycle.callBindRemove(client)
			removeSyncClient(client)
			if (clients.isEmpty) off_!!!(HasBindings)
		}
	}
	private[this] def addSyncClient(client: Module): Unit = if (syncClients.add(client)) {
		client.addSyncServer(this)
		if (isFailed) client.fail(new SyncServerException(this))
		else clientActive(client.is(Active))
	}
	private[this] def removeSyncClient(client: Module): Boolean = syncClients.remove(client) match {
		case true => client.removeSyncServer(this); clientActive(false); true
		case false => false
	}
	private[modules] def addSyncServer(server: Module): Unit = {
		if (syncServers.add(server) && !server.isOperating) serverOperating(false)
	}
	private[modules] def removeSyncServer(server: Module): Unit = {
		if (syncServers.remove(server)) serverOperating(true)
	}
	private[modules] def detectCyclicBinding(server: Module, trace: List[Module]): List[Module] = {
		if (this != server) clients.foreach { client =>
			val res = if (this == client) Nil else if (server == client) client :: trace else client.detectCyclicBinding(server, client :: trace)
			if (res.nonEmpty) return res
		}
		Nil
	}
}




/* REQUEST PROCESSOR */
trait RequestProcessor {
	self: Module =>
	import StateParams._
	//	private[this] val requests = mutable.ArrayBuffer[ModuleRequest[_]]()
	private[this] val requests = mutable.ListBuffer[ModuleRequest[_]]()

	def hasRequests: Boolean = requests.synchronized(requests.nonEmpty)
	def noRequests: Boolean = requests.synchronized(requests.isEmpty)
	protected[this] def serveRequest[T](request: ModuleRequest[T]): FutureX[T] = requestAdd(request)
	protected[this] def cancelRequests(filter: ModuleRequest[_] => Boolean = null): Unit = requests synchronized {
		filter match {
			case null => requests.foreach(_.cancel())
			case _ => requests.withFilter(filter).foreach(_.cancel())
		}
	}
	/* wrappers */
	protected[this] def serveOpt[T](code: => T): Option[T] = {
		if (isAbleToServeNow) Option(code) else None
	}
	protected[this] def serveTry[T](code: => T): Try[T] = {
		if (isAbleToServeNow) Try(code) else Failure(new ModuleServiceException)
	}
	protected[this] def serveAsync[T](code: => T)(implicit asyncContext: FutureContext): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] def serveAsync[T](code: => FutureX[T])(implicit asyncContext: FutureContext, d: DummyImplicit): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	protected[this] def serveAsync[T](code: => Future[T])(implicit asyncContext: FutureContext, d: DummyImplicit, d2: DummyImplicit2 = null): FutureX[T] = {
		requestAdd(new ModuleRequest[T].task(code))
	}
	/* INTERNAL */
	private[this] def requestAdd[T](request: ModuleRequest[T]): FutureX[T] = {
		//DEF
		def addAndCheckFirst(): Boolean = requests synchronized {
			val empty = requests.isEmpty
			requests += request
			empty
		}
		//EXEC
		lifeCycle.callRequestAdding(request)
		request.module = this
		//
		if (request.isDone) requestComplete(request)
		else if (isUnableToServe) request.cancel(new ModuleServiceException)
		else {
			if (addAndCheckFirst()) on_!!!(HasRequests)
			if (isAbleToServeNow) request.activate()
		}
		request
	}
	private[modules] def requestStart(request: ModuleRequest[_]): Unit = {
		// todo count exec requests
	}
	private[modules] def requestComplete(request: ModuleRequest[_]): Unit = {
		requests.synchronized(requests -= request)
		lifeCycle.callRequestRemoved(request)
		if (requests.isEmpty) off_!!!(HasRequests)
	}
	private[modules] def pauseRequests(): Unit = requests.synchronized(requests.foreach(_.deactivate()))
	private[modules] def resumeRequests(): Unit = requests.synchronized(requests.foreach(_.activate()))
	private[modules] def executingRequest: Boolean = requests.synchronized(requests.exists(_.isExecuting))
}




/* STATE PROCESSOR */
trait StateProcessor {
	self: Module =>
	import Module._
	import ModuleState._
	import StateParams._
	private[this] var state: ModuleState = PREPARING
	private[this] var error: Throwable = null
	private[this] var completeOption: CompleteOption = _
	private[this] var stateStartTime = 0L
	private[this] val updateLock = new Object
	private[this] var updateFlag = uFlagNONE
	private[this] var expectUpdate = false
	//
	private[this] var params = 1 << Creating.id | 1 << AllServersOperating.id

	protected[this] def setFailed(e: Throwable): Unit = fail(e)
	protected[this] def setRestful(on: Boolean): Unit = set(Restful, on)
	protected[this] def suspendService(on: Boolean): Unit = {
		if (state == OPERATING) on match {
			case true => pauseRequests(); //	fireEvent(new UnableToServeEvent)
			case false => resumeRequests(); // fireEvent(new AbleToServeEvent)
		}
		set(Suspended, on)
	}
	protected[this] def recover(): Unit = if (state == FAILED && not(Detached)) on_!!!(Recover)

	protected[modules] def getState: ModuleState = state
	def isOperating: Boolean = state == OPERATING
	def isActivating: Boolean = state == ACTIVATING
	def isDeactivating: Boolean = state == DEACTIVATING
	def isFailed: Boolean = state == FAILED
	def isSuspended: Boolean = is(Suspended)
	def isRestful: Boolean = is(Restful)
	def failure: Option[Throwable] = Option(error)

	/* INTERNAL */
//	private[modules] def isAlive: Boolean = not(Destroying)
	private[modules] def isUnableToServe: Boolean = state == FAILED || is(Detached)
	private[modules] def isAbleToServeNow: Boolean = state == OPERATING && not(Suspended) && notUpdating

	private[modules] def set(param: StateParams, v: Boolean, silent: Boolean = false): Unit = setParam(param, v, silent)
	private[modules] def on_!!!(param: StateParams): Unit = setParam(param, true, false)
	private[modules] def on_(param: StateParams): Unit = setParam(param, true, true)
	private[modules] def off_!!!(param: StateParams): Unit = setParam(param, false, false)
	private[modules] def off_(param: StateParams): Unit = setParam(param, false, true)
	private[modules] def is(param: StateParams): Boolean = (params & (1 << param.id)) != 0
	private[modules] def not(param: StateParams): Boolean = (params & (1 << param.id)) == 0

	private[this] def setParam(param: StateParams, isOn: Boolean, silent: Boolean): Unit = {
		val prev = params
		if (isOn) params |= (1 << param.id) else params &= ~(1 << param.id)
		logV(s"[${getClass.getSimpleName}]:  [${(0 to 14).map(n ⇒ if ((params & (1 << n)) == 0) 0 else 1).mkString("")}];  $param= $isOn;  ${if (prev != params && !silent) ">>> " else if (prev != params) "V" else "-"}", tagParam)
		if (prev != params && !silent) updateState()
	}

	private[modules] def updateState(): Unit = {
		// EXEC
		if (nonBusy) {
			update()
//			if (hasFlag(uFlagRESUME)) logV(s"[${getClass.getSimpleName}]:  UPDATE RESUME")
			if (touched) updateState()
		}
		// DEFs
		def update(): Unit = {
			if (expectUpdate) {system.cancelUpdate(); expectUpdate = false}
			val prevState = state
			state match {
				case PREPARING => if (destroy_?) destroy_>> else if (is(Prepared)) prepared_>>
				case RESTING => if (destroy_?) destroy_>> else if (activate_?) activate_>>
				case ACTIVATING => if (complete_?) operate_>>
				case OPERATING => if (deactivate_?) deactivate_>>
				case DEACTIVATING => if (complete_?) rest_>>
				case FAILED => if (abandoned_?) destroy_>> else if (is(Recover)) recover_>>
				case DESTROYED => //todo can module or system do something after destroyed (ex:prepare)
				case _ =>
			}
			if (prevState != state) {
				stateStartTime = system.now
				if (state != DESTROYED) setFlag(uFlagTOUCHED)
			}
			else logW(s"[${getClass.getSimpleName}]:  $prevState -->  X", tagStateX)
		}
		def nonBusy = updateLock.synchronized {
			updateFlag == uFlagNONE || hasFlag(uFlagREPEAT) match {
				case true ⇒ unsetFlag(uFlagREPEAT); setFlag(uFlagBUSY); true
				case false ⇒ setFlag(uFlagTOUCHED); false
			}
		}
		def touched = updateLock.synchronized {
			hasFlag(uFlagTOUCHED) match {
				case true ⇒ unsetFlag(uFlagTOUCHED); setFlag(uFlagREPEAT); true
				case _ ⇒ if (hasFlag(uFlagRESUME) && state == OPERATING && not(Suspended)) resumeRequests()
					updateFlag = uFlagNONE
					false
			}
		}
		def hasFlag(f: Int) = (updateFlag & (1 << f)) != 0
		def setFlag(f: Int) = updateFlag |= 1 << f
		def unsetFlag(f: Int) = updateFlag &= ~(1 << f)
	}

	private[this] def notUpdating: Boolean = updateLock.synchronized {
		updateFlag == uFlagNONE || {
			updateFlag |= 1 << uFlagRESUME
			false
		}
	}
	private[this] def setState(v: ModuleState): Unit = {
		logW(s"[${getClass.getSimpleName}]:  $state -->  $v", tagState)
		state = v
	}

	private[modules] def setConstructed(): Unit = trying {
		on_(Constructed)
		if (internal != null) internal.callConstructed()
	}
	private[modules] def setPrepared(): Unit = trying {
		if (internal != null) internal.callPrepared()
		on_!!!(Prepared)
	}
	private[this] def prepared_>> : Unit = {
		setState(RESTING)
	}
	//todo should it wait when is creating
	private[this] def activate_? : Boolean = changeActive(isUnbound || not(Restful) || isBusy || is(HasClientActive)) && canActivate
	private[this] def canActivate: Boolean = is(AllServersOperating)
	private[this] def isBusy: Boolean = hasRequests && not(Suspended)
	private[this] def activate_>> : Unit = trying {
		setState(ACTIVATING)
		startProgress()
	}
	private[this] def startProgress(): Unit = {
		completeOption = lifeCycle.callStart()
		if (completeOption == CompleteNow) on_(Complete)
	}
	private[this] def complete_? : Boolean = completeOption match {
		case CompleteWhen(isComplete) => trying(isComplete() || {postUpdateState(lifeCycle.backoff); false})
		case _ if is(Complete) => off_(Complete); true
		case _ => false
	}
	private[this] def operate_>> : Unit = trying {
		if (hasSyncServers) off_(AllServersOperating)
		lifeCycle.callComplete()
		if (is(Creating)) off_(Creating)
		setState(OPERATING)
		if (not(Suspended)) resumeRequests()
		syncClients.foreach(_.serverOperating(true))
		//	 if (!servePaused) fireEvent(new AbleToServeEvent)
	}
	private[this] def deactivate_? : Boolean = canDeactivate match {
		case true if is(Delayed) || is(Suspended) || lifeCycle.latency == 0 => off_(Delayed); isBound || detached
		case true => on_(Delayed); postUpdateState(lifeCycle.latency); false
		case _ => off_(Delayed); false
	}
	private[this] def canDeactivate: Boolean = canRest && (isUnbound || (is(Restful) && not(HasClientActive)))
	private[this] def canRest: Boolean = noRequests || (is(Suspended) && !executingRequest)
	private[this] def detached: Boolean = is(Detached) || (system.detach match {
		case true ⇒ on_(Detached); true
		case _ ⇒ false
	})
	private[this] def deactivate_>> : Unit = trying {
		setState(DEACTIVATING)
		startProgress()
		// if (!servePaused && !terminating) fireEvent(new UnableToServeEvent)
	}
	private[this] def rest_>> : Unit = trying {
		setState(RESTING)
		lifeCycle.callComplete()
		changeActive(false)
		asyncContext.stop(true) // todo test no requests lost
	}
	private[this] def abandoned_? : Boolean = isUnbound && is(Constructed) && detached
	private[this] def recover_>> : Unit = {
		setState(RESTING)
		on_(Creating)
		off_(Recover)
		error = null
		syncClients.foreach(m ⇒ if (m.isFailed) m.on_!!!(Recover))
	}
	private[this] def trying[T](code: => T): T = try code catch {
		case err: Throwable => handleError(err); null.asInstanceOf[T]
	}
	private[this] def handleError(err: Throwable): Unit = if (lifeCycle != null) lifeCycle.callFailure(err) match {
		case Some(e) => if (err.ne(e) && e.getCause == null) e.initCause(err); fail(e)
		case None => logE(err, s"Error in [${getClass.getName}] caught but ignored.")
	}
	private[modules] def fail(err: Throwable): Unit = {
		logE(err, s"Module [${getClass.getName}] is failed in state [$state]. ")
		if (error == null) {
			setState(FAILED)
			error = err
			changeActive(false)
			try if (lifeCycle != null) lifeCycle.callFailed() catch loggedE
			cancelRequests()
			if (asyncContext != null) asyncContext.stop()
			syncClients.foreach(_.fail(new SyncServerException(this)))
			// fireEvent(new UnableToServeEvent)
		}
	}
	private[this] def destroy_? : Boolean = (isUnbound && detached) || (isUnbound && is(Creating) && is(Constructed) && !isBusy)
	private[this] def destroy_>> : Unit = {
		setState(DESTROYED)
		try if (internal != null) internal.callDestroyed() catch loggedE
		cancelRequests()
		if (asyncContext != null) asyncContext.exit()
		system.destroyed
	}

	private[modules] def serverOperating(on: Boolean): Unit = {
		if (!on) off_(AllServersOperating) else if (syncServers.forall(_.isOperating)) on_!!!(AllServersOperating)
	}
	private[modules] def clientActive(isOn: Boolean): Unit = if (is(HasClientActive) != isOn) {
		if (isOn || syncClients.forall(_.not(Active))) set(HasClientActive, isOn)
	}
	private[this] def changeActive(isOn: Boolean): Boolean = is(Active) != isOn match {
		case true => set(Active, isOn); syncServers.foreach(_.clientActive(isOn)); isOn
		case _ => isOn
	}

	private[modules] def postUpdateState(delay: Long): Unit = {
		if (expectUpdate) system.cancelUpdate() else expectUpdate = true
		system.postUpdate(delay)
	}
	private[modules] def progressDuration: Long = system.now - stateStartTime
}


/* STATE PARAMS */
private[modules] object StateParams extends Enumeration {
	type StateParams = Value
	/** Up to 32 params */
	val HasBindings,
	HasRequests,
	Restful,
	Suspended,
	HasClientActive,
	AllServersOperating,
	Active, // derivative from above params
	Creating, // indicates primary cycle
	Detached, // indicates primary cycle
	Constructed, // instance constructed
	Prepared, // transition: PREPARING > ACTIVATING
	Complete, //transition: ACTIVATING > OPERATING or DEACTIVATING > RESTING
	Recover, //transition: FAILED > RESTING
	Delayed // delayed deactivation
	= Value
}




/* PROGRESS OPTIONS  */
//object ProgressOption extends Enumeration {
//	val CompleteNow, CompleteManually, PollWithBackoff = Value
//}

object CompleteSelector extends CompleteSelector
class CompleteSelector {
	def now: CompleteOption = CompleteNow
	def manually: CompleteOption = CompleteManually
	def when(isComplete: => Boolean): CompleteOption = CompleteWhen(() => isComplete)
}
class CompleteOption
object CompleteNow extends CompleteOption
object CompleteManually extends CompleteOption
case class CompleteWhen(isComplete: () => Boolean) extends CompleteOption


