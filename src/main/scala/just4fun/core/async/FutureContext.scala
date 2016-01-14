package just4fun.core.async

import just4fun.utils.logger.Logger._

import scala.concurrent.ExecutionContext

trait FutureContext extends ExecutionContext {
	private[this] var exited = false
	private[this] var stopped = true
	protected[this] val owner: FutureContextOwner

	protected[this] def start_inner(): Unit
	protected[this] def clear_inner(): Unit
	protected[this] def stop_inner(softly: Boolean): Unit
	protected[this] def execute_inner(id: Any, delay: Long, r: Runnable): Unit
	protected[this] def execute_inner(r: Runnable): Unit
	protected[this] def cancel_inner(idOrRunnable: Any): Unit

	def isExited: Boolean = synchronized(exited)
	def isStarted: Boolean = synchronized(!stopped)
	def hasPermission(implicit o: FutureContextOwner): Boolean = {
		o == owner || owner == null
	}
	def execute(r: Runnable): Unit = {
		if (start()) execute_inner(r)
	}
	def execute(id: Any, delay: Long, r: Runnable): Unit = {
		if (start()) execute_inner(id, delay, r)
	}
	def execute(id: Any, delayMs: Long, replace: Boolean, runnable: Runnable): Unit = {
		if (replace && id != null) cancel(id)
		if (id == null && delayMs == 0) execute(runnable)
		else execute(id, delayMs, runnable)
	}
	def cancel(idOrRunnable: Any): Unit = synchronized {
		if (!stopped && idOrRunnable != null) cancel_inner(idOrRunnable)
	}
	def start(): Boolean = synchronized {
		if (stopped && !exited) {
			start_inner()
			stopped = false
		}
		!stopped
	}
	def clear()(implicit o: FutureContextOwner): Boolean = synchronized {
		if (!stopped && hasPermission) {clear_inner(); true} else false
	}
	def stop(softly: Boolean = false)(implicit o: FutureContextOwner): Boolean = synchronized {
		if (!stopped && hasPermission) {
			stop_inner(softly)
			stopped = true
		}
		stopped
	}
	def exit(softly: Boolean = false)(implicit o: FutureContextOwner): Boolean = synchronized {
		if (hasPermission && stop(softly)) exited = true
		exited
	}
	override def prepare(): FutureContext = {
		start()
		this
	}
	override def reportFailure(t: Throwable): Unit = logE(t)
}



trait FutureContextOwner {
	implicit protected[this] val contextOwner: FutureContextOwner = this
}