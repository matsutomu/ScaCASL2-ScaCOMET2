package scacomet2

import org.scalatest._


class MachineSpec extends FlatSpec with DiagrammedAssertions {

  "Machine" should " execute All Operand Type " in {
    val machine = new Machine()

    List(0x0000,         // 00: NOP
      0x1411, 0x0000, // 01: LD   GR1,   GR1
      0x1010, 0x000B, // 03: LD   GR1,   #000B
      0x6402, 0x0007, // 05: JUMP #0007, GR2(0)
      0x7001, 0x0000, // 07: PUSH #0000, GR1
      0x7110,         // 09: POP  GR1
      0x8100,         // 10: RET
      0x0041, 0x0042  // 11: A(0x41), B(0x42)
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)

    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // OperandNoArg
      List(0x0002, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // OperandR1R2
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // OperandNoArg
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00), // OperandR_ADR_X
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00), // OperandADR_X
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00 - 1), // OperandR
      List(0x000A, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00))

    assert(machine.stepCount === 0)
    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e.head)
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))
    }
    assert(machine.stepCount === 7)

    // POP  GR1
    assert(machine.memory(machine.gr1.word) === 0)
    machine.step()




  }

  it should " execute Load/Store instructions  " in {
    val machine = new Machine()

    List(0x0000,      // 00: NOP
      0x1010, 0x000A, // 01: LD   GR1, #000B, [0]
      0x1422,         // 03: LD   GR2, GR2
      0x0000,         // 04: NOP
      0x1112, 0x0004, // 05: ST   GR1, #0004, GR2
      0x1201, 0x0000, // 07: LAD  GR0, #0000, GR1
      0x8100,         // 09: RET
      0x0041, 0x0042, // 10: A(0x41), B(0x42)
      0x0043, 0x0044  // 12: C(), D()
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0004, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0,0x41, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0x41,0x41, 0, 0, 0, 0, 0, 0, 0xFF00),
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0x41,0x41, 0, 0, 0, 0, 0, 0, 0xFF00))

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

      if(machine.PR.word >= 7){
        assert(machine.memory(4) === 0x41.toShort)
      } else {
        assert(machine.memory(4) === 0)
      }
    }
  }


  it should " execute ADD/SUB Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1020, 0x0024, // 01(03): LD     GR2, #0024, [0]
      0x1017, 0x0020, // 03(05): LD     GR1, #0020, GR7
      0x2412,         // 05(06): ADDA   GR1, GR2
      0x1030, 0x0020, // 06(08): LD     GR3, #0020, [0]
      0x2431,         // 08(09): ADDA   GR3, GR1
      0x1270, 0x0024, // 09(0B): LAD    GR7, #0024, [0]
      0x2047, 0x0001, // 0B(0D): ADDA   GR4, #0001, GR7
      0x1050, 0x0024, // 0D(0F): LD     GR5, #0024, [0]
      0x1060, 0x0022, // 0F(11): LD     GR6, #0022, [0]
      0x2565,         // 11(12): SUBA   GR6, GR5
      0x2125, 0x0023, // 12(14): SUBA   GR2, #0023, GR5
      0x2235, 0x0023, // 14(16): ADDL   GR3, #0023, GR5
      0x2646,         // 16(17): ADDL   GR4, GR6
      0x2325, 0x0023, // 17(19): SUBL   GR2, #0023, GR5
      0x2735,         // 19(1A): SUBL   GR4, GR6
      0x0000,         // 1A: NOP
      0x0000,         // 1B: NOP
      0x0000,         // 1C: NOP
      0x0000,         // 1D: NOP
      0x0000,         // 1E: NOP
      0x8100,         // 1F: RET
      0x7FFF, 0x0042, // 20: 32767, B(0x42)
      0xFFFF, 0x0044, // 22: C(), D()
      0x0001, 0x0002  // 24: 1, 0
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0006, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x0008, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0009, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0, 0, 0, 0, 0xFF00), // ADDA （ 32767 - 32768 = -1 (0xFFFF))
      List(0x000B, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0, 0, 0,0x0024, 0xFF00), // LAD
      List(0x000D, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0x0002, 0, 0,0x0024, 0xFF00), // ADDA
      List(0x000F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0x0002, 0x0001, 0,0x0024, 0xFF00), // LD
      List(0x0011, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0x0002, 0x0001, 0xFFFF, 0x0024, 0xFF00), // LD
      List(0x0012, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0xFFFF, 0x0002, 0x0001, 0xFFFE, 0x0024, 0xFF00), // SUBA
      List(0x0014, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x8000, 0x0000, 0xFFFF, 0x0002, 0x0001, 0xFFFE, 0x0024, 0xFF00), // SUBA
      List(0x0016, BinaryNumber.One, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x8000, 0x0000, 0x0000, 0x0002, 0x0001, 0xFFFE, 0x0024, 0xFF00), // ADDL
      List(0x0017, BinaryNumber.One, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x8000, 0x0000, 0x0000, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // ADDL
      List(0x0019, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0x0000, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // SUBL
      List(0x001A, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0xFFFF, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // SUBL
      List(0x001B, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0xFFFF, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // NOP(Flag not Change)
      List(0x001C, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0xFFFF, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // NOP(Flag not Change)
      List(0x001D, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0xFFFF, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // NOP(Flag not Change)
      List(0x001E, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0xFFFF, 0xFFFF, 0x0000, 0x0001, 0xFFFE, 0x0024, 0xFF00), // NOP(Flag not Change)
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

    }

    assert(machine.PR.word  === 0x001E)
  }


  it should " execute AND/OR/XOR Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1010, 0x0022, // 01(03): LD     GR1, #0022, [0]
      0x1020, 0x0025, // 03(05): LD     GR2, #0025, [0]
      0x3012, 0x0023, // 05(07): AND    GR1, #0023, GR2
      0x1010, 0x0022, // 07(09): LD     GR1, #0022, [0]
      0x3112, 0x0023, // 09(0B): OR     GR1, #0023, GR2
      0x1010, 0x0022, // 0B(0D): LD     GR1, #0022, [0]
      0x3212, 0x0023, // 0D(0F): XOR    GR1, #0023, GR2
      0x0000,         // 0F(10): NOP
      0x1010, 0x0022, // 10(12): LD     GR1, #0022, [0]
      0x1020, 0x0024, // 12(14): LD     GR2, #0025, [0]
      0x3412,         // 14(15): AND    GR1, GR2
      0x1010, 0x0022, // 15(17): LD     GR1, #0022, [0]
      0x1020, 0x0024, // 17(19): LD     GR2, #0025, [0]
      0x3512,         // 19(1A): OR     GR1, GR2
      0x1010, 0x0022, // 1A(1C): LD     GR1, #0022, [0]
      0x1020, 0x0024, // 1C(1E): LD     GR2, #0025, [0]
      0x3612,         // 1E(1F): XOR    GR1, GR2
      0x8100,         // 1F: RET
      0x7FFF, 0x0042, // 20: 32767, B(0x42)
      0xFFF5, 0x0044, // 22: C(), D()
      0xFFF3, 0x0001  // 24: 1, 0
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0x0000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFF5, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0007, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF1, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // AND
      List(0x0009, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x000B, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF7, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // OR
      List(0x000D, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x000F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0006, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // XOR
      List(0x0010, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0006, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0012, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0014, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0015, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF1, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // AND
      List(0x0017, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0019, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x001A, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF7, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // OR
      List(0x001C, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x001E, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFF5, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0006, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // XOR
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0006, 0xFFF3, 0, 0, 0, 0, 0, 0xFF00), // RET
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

    }

    assert(machine.PR.word  === 0x001F)
  }

  it should " execute CPA/CPL Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1010, 0x0026, // 01(03): LD     GR1, #0026, [0]
      0x1020, 0x002A, // 03(05): LD     GR2, #002A, [0]
      0x4012, 0x0026, // 05(07): CPA    GR1, #0026, GR2
      0x4010, 0x0026, // 07(09): CPA    GR1, #0026, [0]
      0x1010, 0x0027, // 09(0B): LD     GR1, #0027, [0]
      0x4010, 0x0029, // 0B(0D): CPA    GR1, #0029, [0]
      0x0000,         // 0D(0E): NOP
      0x1010, 0x0026, // 0E(10): LD     GR1, #0026, [0]
      0x1020, 0x0027, // 10(12): LD     GR1, #0027, [0]
      0x4412,         // 12(13): CPA    GR1, GR2
      0x4411,         // 13(14): CPA    GR1, GR1
      0x4421,         // 14(15): CPA    GR2, GR1
      0x1010, 0x0026, // 15(17): LD     GR1, #0026, [0]
      0x1020, 0x0027, // 17(19): LD     GR2, #0027, [0]
      0x4120, 0x0026, // 19(1B): CPL    GR2, #0026, [0]
      0x4110, 0x0026, // 1B(1D): CPL    GR1, #0026, [0]
      0x4110, 0x0027, // 1D(1F): CPL    GR1, #0027, [0]
      0x4521,         // 1F(20): CPL    GR2, GR1
      0x4522,         // 20(21): CPL    GR2, GR2
      0x4512,         // 21(22): CPL    GR1, GR2
      0x0000,         // 22(23): NOP
      0x0000,         // 23(24): NOP
      0x0000,         // 24(25): NOP
      0x8100,         // 25: RET
      0x7FFF, 0x8000, // 26: 32767, -32768
      0xFFFF, 0x8001, // 28:    -1, -32767
      0x0001, 0x0001, // 2A:     1, 1
      0x8002, 0x0001  // 2C:     1, 1
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }


    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x0000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x7FFF > 0x8000
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x7FFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x7FFF = 0x7FFF
      List(0x000B, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x000D, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x8000 < 0x8001
      List(0x000E, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x8000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0010, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0012, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0013, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x7FFF > 0x8000
      List(0x0014, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x7FFF = 0x7FFF
      List(0x0015, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPA 0x8000 < 0x7FFF
      List(0x0017, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0019, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x001B, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x8000 > 0x7FFF
      List(0x001D, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x7FFF = 0x7FFF
      List(0x001F, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x7FFF < 0x8000
      List(0x0020, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x8000 > 0x7FFF
      List(0x0021, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x7FFF = 0x7FFF
      List(0x0022, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // CPL 0x7FFF < 0x8000
      List(0x0023, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0024, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0025, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x7FFF, 0x8000, 0, 0, 0, 0, 0, 0xFF00), // NOP

    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))


    }

    assert(machine.PR.word  === 0x0025)
  }


  it should " execute SLA/SRA/SLL/SRL Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1010, 0x0020, // 01(03): LD     GR1, #0020, [0]
      0x1070, 0x0024, // 03(05): LD     GR7, #0024, [0]
      0x5017, 0x0000, // 05(07): SLA    GR1, #0000, GR7
      0x5017, 0x0000, // 07(09): SLA    GR1, #0000, GR7
      0x5117, 0x0001, // 09(0B): SRA    GR1, #0001, GR7
      0x5117, 0x0000, // 0B(0D): SRA    GR1, #0000, GR7
      0x1010, 0x0020, // 0D(0F): LD     GR1, #0020, [0]
      0x0000,         // 0F(10): NOP
      0x5217, 0x0001, // 10(12): SLL    GR1, #0001, GR7
      0x5210, 0x0001, // 12(14): SLL    GR1, #0001, [0]
      0x0000,         // 14(15): NOP
      0x5310, 0x0003, // 15(17): SRL    GR1, #0003, [0]
      0x5310, 0x0001, // 17(19): SRL    GR1, #0001, [0]
      0x1010, 0x0024, // 19(1B): LD     GR1, #0024, [0]
      0x5017, 0x0000, // 1B(1C): SLA    GR1, #0000, GR7
      0x5117, 0x0000, // 1C(1F): SRA    GR1, #0000, GR7
      0x8100,         // 1F: RET
      0xCFFF, 0xCFFD, // 20: 32767, B(0x42)
      0xFFF5, 0x0044, // 22: C(), D()
      0x0001, 0x0001  // 24: 1, 0
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xCFFF, 0x0000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xCFFF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // LD
      List(0x0007, BinaryNumber.One,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0x9FFE, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SLA
      List(0x0009, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xBFFC, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SLA
      List(0x000B, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xEFFF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SRA
      List(0x000D, BinaryNumber.One,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xF7FF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SRA
      List(0x000F, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xCFFF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // LD
      List(0x0010, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xCFFF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // NOP
      List(0x0012, BinaryNumber.One,  BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x3FFC, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SLL
      List(0x0014, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FF8, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SLL
      List(0x0015, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x7FF8, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // NOP
      List(0x0017, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0FFF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SRL
      List(0x0019, BinaryNumber.One, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x07FF, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SRL
      List(0x001B, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // LD
      List(0x001D, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0002, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SLA
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // SRL
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0000, 0, 0, 0, 0, 0x0001, 0xFF00), // RET
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

    }

    assert(machine.PR.word  === 0x001F)
  }



  it should " execute JMI/JNZ/JZE/JUMP/JPL/JOV Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1010, 0x0026, // 01(03): LD     GR1, #0026, [0]
      0x1020, 0x0027, // 03(05): LD     GR2, #0027, [0]
      0x2512,         // 05(06): SUBA   GR1, GR2
      0x6192, 0x0007, // 06(08): JMI    #0007, GR2
      0x0000,         // 08(09): NOP
      0x6282, 0x000B, // 09(0B): JNZ    #0009, GR2
      0x0000,         // 0B(0C): NOP
      0x0000,         // 0C(0D): NOP
      0x2010, 0x0026, // 0D(0F): ADDA   GR1, #0026, [0]
      0x6372, 0x0011, // 0F(11): JZE    #0011, GR2
      0x0000,         // 11(12): NOP
      0x0000,         // 12(13): NOP
      0x6462, 0x0015, // 13(15): JUMP   #0017, GR2
      0x0000,         // 15(16): NOP
      0x0000,         // 16(17): NOP
      0x2010, 0x0026, // 17(19): ADDA   GR1, #0026, [0]
      0x6552, 0x001B, // 19(1B): JPL    #001B, GR2
      0x0000,         // 1B(1C): NOP
      0x0000,         // 1C(1D): NOP
      0x1030, 0x0028, // 1D(1F): LD     GR3, #0028, [0]
      0x2030, 0x0027, // 1F(21): ADDA   GR3, #0026, [0]
      0x6642, 0x0023, // 21(23): JOV    #0024, GR2
      0x0000,         // 23(24): NOP
      0x0000,         // 24(25): NOP
      0x8100,         // 25: RET
      0x0001, 0x0002, // 26: 32767, B(0x42)
      0x7FFE, 0x0044, // 28: C(), D()
      0x0001, 0x0001  // 2A: 1, 0
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // watch flags
    machine.addWatch("OF")
    machine.addWatch("SF")
    machine.addWatch("ZF")

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0006, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // SUBA
      List(0x0009, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // JMI
      List(0x000D, BinaryNumber.Zero,  BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // JNZ
      List(0x000F, BinaryNumber.Zero,  BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x0013, BinaryNumber.Zero,  BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // JZE
      List(0x0017, BinaryNumber.Zero,  BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // JUMP
      List(0x0019, BinaryNumber.Zero,  BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x001D, BinaryNumber.Zero,  BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // JPL
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0x7FFE, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0021, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0x8000, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x0025, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0x8000, 0, 0, 0, 0, 0xFF00), // JOV
      List(0x0025, BinaryNumber.One, BinaryNumber.One, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0x8000, 0, 0, 0, 0, 0xFF00), // RET
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))


      if(machine.PR.word == 0x000F){
        assert(machine.watchInfo().sorted ===
          List("OF=0", "SF=0", "ZF=1"))
      }
      if(machine.PR.word == 0x0006){
        assert(machine.watchInfo().sorted ===
          List("OF=0", "SF=1", "ZF=0"))
      }
      if(machine.PR.word == 0x0021){
        assert(machine.watchInfo().sorted ===
          List("OF=1", "SF=1", "ZF=0"))
      }
    }

    assert(machine.PR.word  === 0x0025)
  }

  it should " execute JMI/JNZ/JZE/JUMP/JPL/JOV Instructions(not jump) " in {
    val machine = new Machine()

    List(0x0000,      // 00(01): NOP
      0x1010, 0x0026, // 01(03): LD     GR1, #0026, [0]
      0x1020, 0x0027, // 03(05): LD     GR2, #0027, [0]
      0x2521,         // 05(06): SUBA   GR2, GR1
      0x6192, 0x0007, // 06(08): JMI    #0007, GR2
      0x2512,         // 08(09): SUBA   GR1, GR2
      0x6282, 0x000B, // 09(0B): JNZ    #0009, GR2
      0x0000,         // 0B(0C): NOP
      0x0000,         // 0C(0D): NOP
      0x2010, 0x0026, // 0D(0F): ADDA   GR1, #0026, [0]
      0x6372, 0x0011, // 0F(11): JZE    #0011, GR2
      0x0000,         // 11(12): NOP
      0x0000,         // 12(13): NOP
      0x6462, 0x0015, // 13(15): JUMP   #0017, GR2
      0x0000,         // 15(16): NOP
      0x0000,         // 16(17): NOP
      0x2110, 0x0027, // 17(19): SUBA   GR1, #0027, [0]
      0x6552, 0x001B, // 19(1B): JPL    #001B, GR2
      0x0000,         // 1B(1C): NOP
      0x0000,         // 1C(1D): NOP
      0x1030, 0x0028, // 1D(1F): LD     GR3, #0028, [0]
      0x2030, 0x0026, // 1F(21): ADDA   GR3, #0026, [0]
      0x6642, 0x0023, // 21(23): JOV    #0024, GR2
      0x0000,         // 23(24): NOP
      0x0000,         // 24(25): NOP
      0x8100,         // 25: RET
      0x0001, 0x0002, // 26: 32767, B(0x42)
      0x7FFE, 0x0044, // 28: C(), D()
      0x0001, 0x0001  // 2A: 1, 0
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0000, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0002, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0006, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // SUBA
      List(0x0008, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // JMI
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // SUBA
      List(0x000B, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // JNZ
      List(0x000C, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x000D, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.One,
        0, 0x0000, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x000F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x0011, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // JZE
      List(0x0012, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0013, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0016, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // JUMP
      List(0x0017, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0x0001, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0019, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // SUBA
      List(0x001B, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // JPL
      List(0x001C, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x001D, BinaryNumber.Zero, BinaryNumber.One, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x001F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFE, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0021, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // ADDA
      List(0x0023, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // JOV
      List(0x0024, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0025, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0025, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0xFFFF, 0x0001, 0x7FFF, 0, 0, 0, 0, 0xFF00), // RET
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

    }

    assert(machine.PR.word  === 0x0025)
  }


  it should " execute Call/Ret Instructions " in {
    val machine = new Machine()

    List(0x0000,      // 00: NOP
      0x8000, 0x0007, // 01: CALL #0007, [0]
      0x8100,         // 03: RET
      0x0000,         // 04: NOP
      0x0000,         // 05: NOP
      0x0000,         // 06: NOP
      0x1001, 0x000A, // 07: LD   GR0, #000A, GR1
      0x8100,         // 09: RET
      0x0041, 0x0042, // 0A: A(0x41), B(0x42)
      0x0043, 0x0044  // 0C: C(), D()
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFEFF), // CALL
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0, 0, 0, 0, 0, 0, 0, 0xFEFF), // LD
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // RET
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // RET
    )

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

      if(machine.PR.word == 0x0007){
        assert(machine.memory(0xFEFF) === 0x0001)
      }

    }
  }


  it should " execute  In / Out Instructions " in {
    val machine = new Machine()

    /* 92 + 92 (184 + 58 = 242 + 14) */
    val testIn = """ !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUXYZ""" + /* 79 - 21 =  58 */
                 """[¥]^_`abcdefghijklmnopqrstuxyz{|}~""" + /* 55 - 21 = 34 */
                 """ !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUXYZ""" + /* 79 - 21 =  58 */
                 """[¥]^_`abcdefghijklmnopqrstuxyz{|}~""" + /* 55 - 21 = 34 */
                 """ !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUXYZ""" + /* 79 - 21 =  58 */
                 """0123456789abcde""" /* 36 - 21 = 15 */

    machine.read(testIn)

    val instructions =
        List(0x0000,              // 00: NOP
          0x9000, 0x000A, 0x010A, // 01: IN   0x000A   0x000D
          0x9100, 0x000A, 0x010A, // 04: OUT  0x000A   0x000D
          0x0000,         // 07: NOP
          0x0000,         // 08: NOP
          0x8100,         // 09: RET
        ) ::: List.fill(257)(0x0000)

    instructions.zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0004, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // IN
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // OUT
      List(0x0008, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // RET
    )

    assert(machine.ouput === "")

    for(e <- registers){
      machine.step()
      assert(machine.PR.word  === e(0))
      assert(machine.OF       === e(1))
      assert(machine.SF       === e(2))
      assert(machine.ZF       === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))
    }

    assert(machine.memory(0x000A) === 0x0020)
    assert(machine.memory(0x000B) === 0x0021)
    assert(machine.memory(0x0109) === 0x0064)
    assert(machine.memory(0x010A) === 0x0100) /* Length : 256 */

    assert(machine.ouput() === testIn.substring(0, 256))

  }



  it should " execute Rpush/Rpop Instructions " in {
    val machine = new Machine()

    List(0x0000, // 00: NOP
      0x1000, 0x0023, // 01: LD   GR0 #0020, [0]
      0x1010, 0x0024, // 03: LD   GR1 #0021, [0]
      0x1020, 0x0025, // 05: LD   GR2 #0022, [0]
      0x1030, 0x0026, // 07: LD   GR3 #0023, [0]
      0x1040, 0x0027, // 09: LD   GR4 #0024, [0]
      0x1050, 0x0028, // 0B: LD   GR5 #0025, [0]
      0x1060, 0x0029, // 0D: LD   GR6 #0026, [0]
      0x1070, 0x002A, // 0F: LD   GR7 #0027, [0]
      0xa000, // 12: Rpush
      0x1110, 0xFEF9, // 13:
      0x1120, 0xFEFA, // 15:
      0x1130, 0xFEFB, // 17:
      0x1140, 0xFEFC, // 19:
      0x1150, 0xFEFD, // 1B:
      0x1160, 0xFEFE, // 1D:
      0x1170, 0xFEFF, // 1F:
      0xa100, // 20: Rpop
      0x0000, // 21: NOP
      0x8100, // 22: RET
      0x0041, 0x0042, // 23: A(0x41), B(0x42)
      0x0043, 0x0044, // 25: C(), D()
      0x0045, 0x0046, // 27: C(), D()
      0x0047, 0x0048, // 29: C(), D()
      0x0049, 0x004A, // 2B: C(), D()
      0x004B, 0x004C, // 2D: C(), D()
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    // PR(next), OF, SF, ZF,
    // GR0, GR1, GR2, GR3, GR4, GR5, GR6, GR7, GR8(StackPointer)
    val registers = List(
      List(0x0001, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // NOP
      List(0x0003, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0, 0, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0005, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0007, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0, 0, 0, 0, 0, 0xFF00), // LD
      List(0x0009, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0, 0, 0, 0, 0xFF00), // LD
      List(0x000B, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0, 0, 0, 0xFF00), // LD
      List(0x000D, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0, 0, 0xFF00), // LD
      List(0x000F, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0, 0xFF00), // LD
      List(0x0011, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFF00), // LD
      List(0x0012, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // RPUSH
      List(0x0014, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x0016, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x0018, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x001A, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x001C, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x001E, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x0020, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048, 0xFEF9), // ST
      List(0x0021, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0048, 0x0047, 0x0046, 0x0045, 0x0044, 0x0043, 0x0042, 0xFF00), // Rpop
      List(0x0022, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0048, 0x0047, 0x0046, 0x0045, 0x0044, 0x0043, 0x0042, 0xFF00), // NOP
      List(0x0022, BinaryNumber.Zero, BinaryNumber.Zero, BinaryNumber.Zero,
        0x0041, 0x0048, 0x0047, 0x0046, 0x0045, 0x0044, 0x0043, 0x0042, 0xFF00), // RET

    )

    machine.addWatch("PR")
    machine.addWatch("OF")
    machine.addWatch("SF")
    machine.addWatch("ZF")
    machine.addWatch("GR0")
    machine.addWatch("GR1")
    machine.addWatch("GR2")
    machine.addWatch("GR3")
    machine.addWatch("GR4")
    machine.addWatch("GR5")
    machine.addWatch("GR6")
    machine.addWatch("GR7")
    machine.addWatch("GR8")
    machine.addWatch("1")
    machine.addWatch("#FFFF")

    for (e <- registers) {
      machine.step()
      assert(machine.PR.word === e(0))
      assert(machine.OF === e(1))
      assert(machine.SF === e(2))
      assert(machine.ZF === e(3))
      assert(machine.gr0.word === e(4))
      assert(machine.gr1.word === e(5))
      assert(machine.gr2.word === e(6))
      assert(machine.gr3.word === e(7))
      assert(machine.gr4.word === e(8))
      assert(machine.gr5.word === e(9))
      assert(machine.gr6.word === e(10))
      assert(machine.gr7.word === e(11))
      assert(machine.gr8.word === e(12))

      if (machine.PR.word == 0x0012) {
        assert(machine.gr8.word === 0xFEF9)
        assert(machine.memory(0xFEF9) === 0x0048)
        assert(machine.memory(0xFEFA) === 0x0047)
        assert(machine.memory(0xFEFB) === 0x0046)
        assert(machine.memory(0xFEFC) === 0x0045)
        assert(machine.memory(0xFEFD) === 0x0044)
        assert(machine.memory(0xFEFE) === 0x0043)
        assert(machine.memory(0xFEFF) === 0x0042)
        assert(machine.memory(0xFF00) === 0x0000)
      }

      if (machine.PR.word == 0x0020) {
        assert(machine.gr8.word === 0xFEF9)
        assert(machine.memory(0xFEF9) === 0x0042)
        assert(machine.memory(0xFEFA) === 0x0043)
        assert(machine.memory(0xFEFB) === 0x0044)
        assert(machine.memory(0xFEFC) === 0x0045)
        assert(machine.memory(0xFEFD) === 0x0046)
        assert(machine.memory(0xFEFE) === 0x0047)
        assert(machine.memory(0xFEFF) === 0x0048)
        assert(machine.memory(0xFF00) === 0x0000)
      }
    }

    var result = machine.watchInfo().sorted
    assert(result(0) === "#0001=#1000")
    assert(result(1) === "#FFFF=#0000")
    assert(result(2) === "GR0=#0041")
    assert(result(3) === "GR1=#0048")
    assert(result(4) === "GR2=#0047")
    assert(result(5) === "GR3=#0046")
    assert(result(6) === "GR4=#0045")
    assert(result(7) === "GR5=#0044")
    assert(result(8) === "GR6=#0043")
    assert(result(9) === "GR7=#0042")
    assert(result(10) === "GR8=#FF00")
    assert(result(11) === "OF=0")
    assert(result(12) === "PR=#0022")
    assert(result(13) === "SF=0")
    assert(result(14) === "ZF=0")

    machine.delWatch("1")
    result = machine.watchInfo().sorted
    assert(result(0) === "#FFFF=#0000")
    assert(result(1) === "GR0=#0041")
    assert(result(2) === "GR1=#0048")
    assert(result(3) === "GR2=#0047")
    assert(result(4) === "GR3=#0046")
    assert(result(5) === "GR4=#0045")
    assert(result(6) === "GR5=#0044")
    assert(result(7) === "GR6=#0043")
    assert(result(8) === "GR7=#0042")
    assert(result(9) === "GR8=#FF00")
    assert(result(10) === "OF=0")
    assert(result(11) === "PR=#0022")
    assert(result(12) === "SF=0")
    assert(result(13) === "ZF=0")

    machine.delWatch("#FFFF")
    machine.delWatch("GR0")
    machine.delWatch("GR1")
    machine.delWatch("GR2")
    machine.delWatch("GR3")
    machine.delWatch("GR4")
    machine.delWatch("GR5")
    machine.delWatch("GR6")
    machine.delWatch("GR7")
    machine.delWatch("GR8")
    machine.delWatch("OF")
    machine.delWatch("SF")
    machine.delWatch("ZF")
    machine.delWatch("PR")
    result = machine.watchInfo().sorted
    assert(result === List())
  }

  it should " Add/Print/Delete BreakPoints "  in {
    val machine = new Machine()

    machine.addBreakPoint(5)
    assert(machine.breakPointInfo() === List("   1 : #0005"))
    assert(machine.containsBreakPoint(1) === false)
    assert(machine.containsBreakPoint(5) === true)

    machine.addBreakPoint(1)
    assert(machine.breakPointInfo() === List("   1 : #0001","   2 : #0005"))
    assert(machine.containsBreakPoint(1) === true)
    assert(machine.containsBreakPoint(5) === true)

    machine.addBreakPoint(3)
    assert(machine.breakPointInfo() === List("   1 : #0001"
      ,"   2 : #0003","   3 : #0005"))
    assert(machine.containsBreakPoint(1) === true)
    assert(machine.containsBreakPoint(2) === false)
    assert(machine.containsBreakPoint(3) === true)
    assert(machine.containsBreakPoint(5) === true)

    machine.deleteBreakPoint(2)
    assert(machine.breakPointInfo() === List("   1 : #0001"
      ,"   2 : #0005"))
    assert(machine.containsBreakPoint(1) === true)
    assert(machine.containsBreakPoint(5) === true)

    machine.deleteBreakPoint(2)
    assert(machine.breakPointInfo() === List("   1 : #0001"))
    assert(machine.containsBreakPoint(1) === true)
    assert(machine.containsBreakPoint(5) === false)

    machine.deleteBreakPoint(1)
    assert(machine.breakPointInfo() === List.empty)
    assert(machine.containsBreakPoint(1) === false)
    assert(machine.containsBreakPoint(5) === false)


  }


  it can "disassemble " in {
    val machine = new Machine()

    List(0x0000,         // 00: NOP
      0x1411, 0x0000, // 01: LD   GR1,   GR1
      0x1012, 0x000B, // 03: LD   GR1,   #000B
      0x6402, 0x0007, // 05: JUMP #0007, GR2(0)
      0x7001, 0x0000, // 07: PUSH #0000, GR1
      0x7110,         // 09: POP  GR1
      0x9000, 0x000A, 0x010A, // 10: IN   0x000A   0x000D
      0x9100, 0x000A, 0x010A, // 13: OUT  0x000A   0x000D
      0x8100,         // 16: RET
      0x0041, 0x0042  // 17: A(0x41), B(0x42)
    ).zipWithIndex.foreach { case (e: Int, i: Int) =>
      machine.memory(i) = e
    }

    assert(machine.disassemble(0, 1) ===
      List("#0000: #0000               NOP"))

    assert(machine.disassemble(0, 2) ===
      List("#0000: #0000               NOP",
           "#0001: #1411               LD       GR1, GR1"))

    assert(machine.disassemble(0, 10) ===
      List("#0000: #0000               NOP",
        "#0001: #1411               LD       GR1, GR1",
        "#0002: #0000               NOP",
        "#0003: #1012 #000B         LD       GR1, #000B, GR2",
        "#0005: #6402 #0007         JUMP     #0007, GR2",
        "#0007: #7001 #0000         PUSH     #0000, GR1",
        "#0009: #7110               POP      GR1",
        "#000A: #9000 #000A #010A   IN       #000A, #010A",
        "#000D: #9100 #000A #010A   OUT      #000A, #010A",
        "#0010: #8100               RET")

    )
  }


}
