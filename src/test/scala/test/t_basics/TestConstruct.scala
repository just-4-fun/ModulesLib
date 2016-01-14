package test.t_basics

import scala.collection.mutable
import scala.util.{Success, Failure}
import test._
import just4fun.core.async.{FutureContext, FutureX}
import just4fun.core.modules._
import just4fun.utils.logger.Logger._

object TestConstruct extends TestApp[Mc1, Mc2, Mc3, Mc4, Mc5] {
	import HitPoints._
	
	val opt1 = Opt()
	val opt2 = Opt()
	val opt3 = Opt()
	val opt4 = Opt()
	val opt5 = Opt()
	var failOp = 0
	
	val auto = false
//	val auto = true
	
	if (auto) {
//		autoTest(t1, t2)
		autoTest(t1)
	}
	else manualTest(true, {
		case (s1, com, s2) ⇒
			val n1 =if (s1.isEmpty) 0 else s1.toInt
			val n2 =if (s2.isEmpty) 0 else s2.toInt
			com match {
				case "optb" => n1 match {
					case 0 => opt1.bindOpt = n2; opt2.bindOpt = n2; opt3.bindOpt = n2; opt4.bindOpt = n2; opt5.bindOpt = n2
					case 1 => opt1.bindOpt = n2
					case 2 => opt2.bindOpt = n2
					case 3 => opt3.bindOpt = n2
					case 4 => opt4.bindOpt = n2
					case 5 => opt5.bindOpt = n2
				}
				case "optf" => n1 match {
					case 0 => opt1.failOpt = n2; opt2.failOpt = n2; opt3.failOpt = n2; opt4.failOpt = n2; opt5.failOpt = n2
					case 1 => opt1.failOpt = n2
					case 2 => opt2.failOpt = n2
					case 3 => opt3.failOpt = n2
					case 4 => opt4.failOpt = n2
					case 5 => opt5.failOpt = n2
				}
				case "optF" => failOp = n2
				case _ => println(s"Command not found: '${s1 + com + s2}'")
			}
	})
	val com = "optb1 optF2 /= 1s 2s 3s 4s 5s ///1000 1sx 2sx 3sx 4sx 5sx /"

	lazy val t1 = new AutoTest("1", "optb1 /= 1s 2s 3s 4s 5s /=/ ///1000 /= 1sx 2sx 3sx 4sx 5sx /") {
		override def assertions: Seq[Assertion] = Seq(
			ModCreate(1) >>>> ModConstr(1) >>>> ModBindAdd(1) >>>> ModPrepare(1) >>>> ModActStart(1) >>>> ModActCompl(1)>>>> ModDeactStart(1) >>>> ModDeactCompl(1) >> ModDestroy(1) >?
		)
	}
	
	case class Opt(var bindOpt: Int = 0, var failOpt: Int = 0)
}


class Mc1 extends Module1 {
	import TestConstruct._
	if (opt1.failOpt == 1) throw new Exception("OOPS...")
	if (opt1.bindOpt == 1) bind[MFailed] else if (opt1.bindOpt == 2) bindSync[MFailed]
}
class Mc2 extends Module2 {
	import TestConstruct._
	if (opt2.failOpt == 1) throw new Exception("OOPS...")
	if (opt2.bindOpt == 1) bind[MFailed] else if (opt2.bindOpt == 2) bindSync[MFailed]
}
class Mc3 extends Module3 {
	import TestConstruct._
	if (opt3.failOpt == 1) throw new Exception("OOPS...")
	if (opt3.bindOpt == 1) bind[MFailed] else if (opt3.bindOpt == 2) bindSync[MFailed]
}
class Mc4 extends Module4 {
	import TestConstruct._
	if (opt4.failOpt == 1) throw new Exception("OOPS...")
	if (opt4.bindOpt == 1) bind[MFailed] else if (opt4.bindOpt == 2) bindSync[MFailed]
}
class Mc5 extends Module5 {
	import TestConstruct._
	if (opt5.failOpt == 1) throw new Exception("OOPS...")
	if (opt5.bindOpt == 1) bind[MFailed] else if (opt5.bindOpt == 2) bindSync[MFailed]
}

class MFailed extends Module {
	import TestConstruct._
	override implicit protected val asyncContext: FutureContext = if (failOp == 1) null else system.asyncContext
	if (failOp == 2) throw new Exception("OOPS...")
	override protected[this] val lifeCycle = new LifeCycle {
		if (failOp == 3) throw new Exception("OOPS...")
	}
	override protected[this] val internal = new InternalCallbacks {
		if (failOp == 4) throw new Exception("OOPS...")
	}
}
