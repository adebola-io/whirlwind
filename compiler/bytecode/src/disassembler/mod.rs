use crate::{
    from_text::bytecode_from_text,
    reader::{BytecodeReader, DisAsmBytecodeReader},
    BytecodeObject, Opcode, RegisterGroup, DISASSEMBLER_SPACING,
};
use utils::terminal::{clear, Colorable};

/// Disassembles a bytecode object into a readable string.
pub fn disassemble(object: &BytecodeObject) -> String {
    let mut string = String::new();

    clear();
    string.push_str(
        &"----------------------\nwhirlwindc v.0.0.0\n----------------------\n"
            .color()
            .italic()
            .gray()
            .str(),
    );

    let mut i = 0;
    for callable in &object.functions {
        let mut label = String::new();
        label.push_str("[callable] ");
        label.push_str(&callable.name);
        label.push_str("(");
        for (idx, _) in (0..callable.param_count).enumerate() {
            label.push_str("param");
            label.push_str(&idx.to_string());
            if idx + 1 == callable.param_count {
                label.push_str(", ");
            }
        }
        label.push_str("):\n");
        string.push_str(&label.color().bright_blue().bold().underline().str());

        let bytes = &object.instructions[callable.start..=callable.end];
        let mut reader = DisAsmBytecodeReader::from(object.instructions.as_slice());
        reader.pc = callable.start;
        while reader.pc <= callable.end {
            disassemble_instruction(&mut reader, &mut string, &object);
            string.push('\n');
        }
        string.push('\n');
    }
    string
}

/// Disassembles a bytecode instruction from a byte stream.
fn disassemble_instruction(
    reader: &mut DisAsmBytecodeReader<'_>,
    string: &mut String,
    object: &BytecodeObject,
) {
    let opcode: Opcode = reader.next_byte().into();
    string.push_str("    ");
    let opcode_str = format!("{opcode:?}").to_ascii_lowercase();
    let len = -(f32::log10(reader.pc as f32).floor()) as isize + 8;
    let instruction_address = "0".repeat(len as usize);

    string.push_str(&format!(
        "{}{}     {}",
        instruction_address.color().gray(),
        reader.pc.to_string().color().gray(),
        opcode_str.color().pink()
    ));
    string.push_str(&" ".repeat(DISASSEMBLER_SPACING - opcode_str.len()));
    let mut output = String::new();
    match opcode {
        Opcode::LoadInt8 => disassemble_loadint8_params(reader, &mut output),
        Opcode::LoadInt16 => todo!(),
        Opcode::LoadFloat32 => todo!(),
        Opcode::LoadFloat64 => disassemble_loadfloat64_params(reader, &mut output),
        Opcode::LoadBool => todo!(),
        Opcode::LoadFunctionPtr => disassemble_load_func_params(reader, &mut output, object),
        Opcode::MoveInt8 => todo!(),
        Opcode::MoveInt16 => todo!(),
        Opcode::MoveFloat32 => todo!(),
        Opcode::MoveFloat64 => todo!(),
        Opcode::MoveBool => todo!(),
        Opcode::MoveAddr => todo!(),
        Opcode::MoveEther => todo!(),
        Opcode::StoreInt8 => todo!(),
        Opcode::StoreInt16 => todo!(),
        Opcode::StoreFloat32 => todo!(),
        Opcode::StoreFloat64 => todo!(),
        Opcode::StoreBool => todo!(),
        Opcode::StoreFunctionPtr => todo!(),
        Opcode::StoreAddr => todo!(),
        Opcode::RetrieveInt8 => todo!(),
        Opcode::RetrieveInt16 => todo!(),
        Opcode::RetrieveFloat32 => todo!(),
        Opcode::RetrieveFloat64 => todo!(),
        Opcode::RetrieveBool => todo!(),
        Opcode::RetrieveFunctionPtr => todo!(),
        Opcode::RetrieveAddr => todo!(),
        Opcode::MovRetVal => disassemble_return_move_params(reader, &mut output),
        Opcode::Div
        | Opcode::Mod
        | Opcode::RightShift
        | Opcode::LeftShift
        | Opcode::Add
        | Opcode::Sub
        | Opcode::Mul => disassemble_arith_params(reader, &mut output),
        Opcode::EqInt8 => todo!(),
        Opcode::EqInt16 => todo!(),
        Opcode::EqFloat32 => todo!(),
        Opcode::EqFloat64 => todo!(),
        Opcode::EqBool => todo!(),
        Opcode::EqFunctionPtr => todo!(),
        Opcode::EqAddr => todo!(),
        Opcode::Negate => todo!(),
        Opcode::JumpIfTrue => todo!(),
        Opcode::JumpConditional => todo!(),
        Opcode::LoopFor => todo!(),
        Opcode::Stall => todo!(),
        Opcode::BreakLoop => todo!(),
        Opcode::Goto => todo!(),
        Opcode::Call => disassemble_call_params(reader, &mut output),
        Opcode::Return => {}
        Opcode::NewInstanceValueA => todo!(),
        Opcode::StoreValueAToFrame => todo!(),
        Opcode::NewInstanceValueB => todo!(),
        Opcode::NewArrayAddrA => todo!(),
        Opcode::NewArrayAddrE => todo!(),
        Opcode::GetPropertyOffset => todo!(),
        Opcode::MoveToRet => disassemble_return_move_params(reader, &mut output),
        Opcode::SpawnSeq => todo!(),
        Opcode::SyncSeq => todo!(),
        Opcode::HaltSeq => todo!(),
        Opcode::Invoke => todo!(),
        Opcode::Exit => todo!(),
    }
    string.push_str(&output.color().cyan().str());
}

fn disassemble_return_move_params(reader: &mut DisAsmBytecodeReader<'_>, output: &mut String) {
    let group = RegisterGroup::from_byte(reader.next_byte());
    let group_str = format!("{group:?}").to_ascii_lowercase();
    let destination = reader.next_byte();
    let data = format!("{group_str}, @r{destination}");
    output.push_str(&data);
}

fn disassemble_arith_params(reader: &mut DisAsmBytecodeReader<'_>, output: &mut String) {
    let group = RegisterGroup::from_byte(reader.next_byte());
    let group_str = format!("{group:?}").to_ascii_lowercase();
    let result = reader.next_byte();
    let left = reader.next_byte();
    let right = reader.next_byte();
    let data = format!("{group_str}, @r{result},  @r{left},  @r{right}");
    output.push_str(&data);
}

fn disassemble_loadint8_params(reader: &mut DisAsmBytecodeReader<'_>, output: &mut String) {
    let register = reader.next_byte();
    let value = reader.next_byte();
    output.push_str(&format!("@r{register},  [{value}]"));
}

fn disassemble_loadfloat64_params(reader: &mut DisAsmBytecodeReader<'_>, output: &mut String) {
    let register = reader.next_byte();
    let value = f64::from_be_bytes(reader.next_eight_bytes());
    output.push_str(&format!("@r{register},  [{value}]"));
}

fn disassemble_call_params(reader: &mut DisAsmBytecodeReader<'_>, output: &mut String) {
    let register = reader.next_byte();
    output.push_str(&format!("@r{register}"));
}

fn disassemble_load_func_params(
    reader: &mut DisAsmBytecodeReader<'_>,
    output: &mut String,
    object: &BytecodeObject,
) {
    let register = reader.next_byte();
    output.push_str(&format!("@r{register}"));
    let id = u32::from_be_bytes(reader.next_four_bytes()) as usize;
    output.push_str(&format!("  ({})", &object.functions[id].name));
}

#[test]
fn disassemble_text() {
    let object = bytecode_from_text(
        "
    module main;

    function main() {
        doMoreMath();
    }
    function doMath() -> UInt8 {
        return 1 + 2 + 3;
    }
    function doMoreMath() -> Float {
        return doMath();
    }
    ",
    )
    .unwrap();

    println!("{}", disassemble(&object));
}
