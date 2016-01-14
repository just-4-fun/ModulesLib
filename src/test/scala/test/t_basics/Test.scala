package test.t_basics

import scala.collection.mutable
import scala.util.{Success, Failure}
import test._
import just4fun.core.async.{FutureContext, FutureX}
import just4fun.core.modules._
import just4fun.utils.logger.Logger._


object Test extends TestApp[M1, M2, M3, M4, M5] {
	import HitPoints._
	
	val auto = false
//	val auto = true

	if (auto) {
//		autoTest(t1, t2)
		autoTest(t1)
	}
	else manualTest(true, {
		case (s1, com, s2) ⇒ println(s"Command not found: '${s1 + com + s2}'")
	})

	lazy val t2 = new AutoTest("2", "1s 1sx") {
		system.prepareDelay = 1000
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModBindRemove(1) >> ModDestroy(1)
			  >> SysModDestroy() >> SysFinish() >?
		)
	}
	lazy val t1 = new AutoTest("1", "1s 1sx") {
		override def assertions: Seq[Assertion] = Seq(
			SysStart() >> ModCreate(1) >> ModConstr(1) >> ModBindAdd(1) >> SysModPrepare() >> ModPrepare(1) >> ModActStart(1, true) >@ 1000
			  >>>> ModActProgress(1, true) >> ModActCompl(1, false) >@ 1000 >> ModDeactStart(1, true) >@ 1000 >>>> ModDeactProgress(1, true) >> ModDeactCompl(1, true) >> ModDestroy(1) >> SysModDestroy() >> SysFinish() >?,
			ModBindAdd(1) >>>> ModBindRemove(1) >>>> ModDeactStart(1, true) >?
		)
	}

}

class M1 extends Module1
class M2 extends Module2
class M3 extends Module3
class M4 extends Module4
class M5 extends Module5
