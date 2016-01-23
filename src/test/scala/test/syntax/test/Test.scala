package test.syntax.test

import test.Module1
import test.syntax.{ModuleX1, SystemX1}

object Test {
	val sys = new SystemX1
	sys.asyncContext
	sys.isSystemStopped
	sys.isSystemStopping
	sys.hasModule
	sys.startModule
	sys.stopModule

	val mod = new ModuleX1
	mod.isAbleToServe
	mod.isAbleToServeNow
	mod.info.state
	mod.info.isPreparing
	mod.info.isOperating
	mod.info.isActivating
	mod.info.isDeactivating
	mod.info.isResting
	mod.info.isFailed
	mod.info.isUnavailable
	mod.info.isRestful
	mod.info.isSuspended
	mod.info.failure
}

