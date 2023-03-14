package wacc.Optimisations

// import wacc.FinalIR._
// import wacc.AssemblerTypes._

object PeepholeOptimisation {
  def PeepholeOptimise(code: List[FinalIR]): List[FinalIR] = {
    // Apply peephole optimisations trying to decrease length the most early on
    code.removeDeadCode()
        .slideAndFilter(isRedundant)
        .filterNot(isNullOp)
        .map(strengthReduction)
  }

  // Convert higher processing time instructions to lower cost ones
  def strengthReduction(instr: FinalIR): FinalIR = {
    instr match {
        // src*(2^x) => src << x
        case Mul(cond, None(), op1, ImmediateInt(x), dst) if (op1 equals dst) 
            && isPower2(x) => Mov(cond, LogicalShiftLeft(dst, Right(getPower2(x))), dst)
        case _ => instr
    }
  }

//   // True if x is a power of 2
//   def isPower2(x: Int): Boolean = {
//     // Using the fact that if x is a power of 2, x & (x-1) = 0
//     x != 0 && (x & (x-1)) == 0
//   }

//   // Returns n where x = 2^n
//   def getPower2(x: Int): Int = {
//     assert(isPower2(x))
//     var tmp = x
//     var n = 0
//     while (tmp != 0) {
//         tmp -= 2
//         n += 1
//     }
//     n
//   }

  // Identify if the instruction can be removed entirely
  def isNullOp(instr: FinalIR): Boolean = {
    instr match {
        // Mov src->src
        case Mov(_, src, dst)
            if src equals dst => true
        // Str/Ldr src->src
        case Str(condition, src, operand, dst) => false 
        case StrPre(condition, src, operand, dst) => false
        case Ldr(condition, src, operand, dst) => false
        // Add/Sub with 0
        case Add(_, None(), op1, ImmediateInt(0), dst) if (op1 equals dst) => true
        case Add(_, None(), ImmediateInt(0), op2, dst) if (op2 equals dst) => true
        case Sub(_, None(), op1, ImmediateInt(0), dst) if (op1 equals dst) => true
        case Sub(_, None(), ImmediateInt(0), op2, dst) if (op2 equals dst) => true
        // Mul with 1
        case Mul(_, None(), op1, ImmediateInt(1), dst) if (op1 equals dst) => true
        case Mul(_, None(), ImmediateInt(1), op2, dst) if (op2 equals dst) => true
        case Smull(_, None(), src, op1, ImmediateInt(1), dst) if (src equals dst) => false 
        case Smull(_, None(), src, ImmediateInt(1), op2, dst) if (src equals dst) => false
        // Empty push/pop
        case Push(_, List()) => true
        case Pop(_, List()) => true
        case _ => false
    }
  }

  // Identify consecutive intructions that contradict each other
  def isRedundant(instr1: FinalIR, instr2: FinalIR): Boolean = {
    instr1 match {
        // Mov back and forth
        case Mov(cond1, src1, dst1) => {
            instr2 match {
                case Mov(cond2, dst2, src2) => 
                    cond1.equals(cond2) && dst1.equals(dst2) && src1.equals(src2)
                case _ => false
            }
        }
        // Push then Pop
        case Push(pushCond, pushList) => {
            instr2 match {
                case Pop(popCond, popList) =>
                    pushCond.equals(popCond) && pushList.equals(popList)
                case _ => false
            }
        }
        // Pop then Push
        case Pop(popCond, popList) => {
            instr2 match {
                case Push(pushCond, pushList) =>
                    pushCond.equals(popCond) && pushList.equals(popList)
                case _ => false
            }
        }
        // Double Str
        case str1: Str => {
            instr2 match {
                case str2: Str => str1.equals(str2)
                case _ => false
            }
        }
        // Double StrPre
        case str1: StrPre => {
            instr2 match {
                case str2: StrPre => str1.equals(str2)
                case _ => false
            }
        }
        case _ => false
    }
  }

  // Implicit class allows for `code.slideAndFilter(func)`
  implicit class slidableList[FinalIR](code: List[FinalIR]) {
    // Slide over a list filtering consecutive elements using func(a,b)
    def slideAndFilter(func: (FinalIR, FinalIR) => Boolean): List[FinalIR] = {
        var lastRemovedIndex = -2
        var currentIndex = 0
        val reducedCode = code.sliding(2).toList.zipWithIndex.filter(pair => pair match {
            case (List(instr1, instr2), index) => {
                currentIndex = index
                // If a pair was 'removed' previously, then we skip the next pair
                if (lastRemovedIndex + 1 == index){
                    false
                } else if (func(instr1, instr2)) {
                    lastRemovedIndex = index
                    false
                } else {
                    true
                }
            }
            case (_, index) => {
                currentIndex = index
                true
            }
        }).map(elem => elem match {
            case (list, _) => list.head
        })
        // Add the last instruction at the end if the last tuple wasn't filtered
        if (currentIndex == lastRemovedIndex) {
            reducedCode
        } else {
            reducedCode.appended(code.last)
        }
    }

    def removeDeadCode(): List[FinalIR] = {
        // Get the indexes of instructions to remove
        val toRemove: List[Int] = code.sliding(4).zipWithIndex.filter(quad => quad match {
            case (List(
                Pop("", List(fp1, pc1)),
                Mov("", ImmediateInt(0), r01),
                Mov("", fp2, sp1),
                Pop("", List(fp3, pc2))), _) => 
                    fp1.equals(fp) && pc1.equals(pc) && r01.equals(r0) && 
                    fp2.equals(fp) && sp1.equals(sp) && fp3.equals(fp) && pc2.equals(pc)
            case _ => false
        }).map(quad => quad match {
            case (_, index) => index
        }).toList

        // Remove the EndFunc dead code from input
        code.zipWithIndex.filterNot(tuple => tuple match {
            case (instr, index) => toRemove.contains(index - 1) || 
                                    toRemove.contains(index - 2) || 
                                    toRemove.contains(index - 3)
            case _ => false
        }).map(tuple => tuple match {
            case (instr, _) => instr
        })
    }
  } 

// }
