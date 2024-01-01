// Node.js script to generate conversions between opcodes and bytes
// and spacing for the disassembler.

const fs = require("fs");

const data = fs.readFileSync(__dirname + "/opcode.txt").toString();
const codes = data
   .split(/\n/)
   .slice(1, -1)
   .map((line) => line.trim())
   .filter((line) => !(line.startsWith("//") || line == ""))
   .map((line) => line.slice(0, -1));

let toByte = `
impl From<Opcode> for u8 {
    fn from(value: Opcode) -> Self {
        match value {
            ${codes
               .map((code, index) => {
                  return `Opcode::${code} => ${index},\n`;
               })
               .join("")}
        }
    }
}
`;

let toCode = `
impl From<u8> for Opcode {
    fn from(value: u8) -> Self {
        match value {
            ${codes
               .map((code, index) => {
                  return `${index} => Self::${code},\n`;
               })
               .join("")}
            _ => panic!("Undefined opcode conversion. No opcode maps to value {value}.")
        }
    }
}
`;
// console.log(toByte + "\n" + toCode);

let longestOpcode = codes.reduce((acc, value) =>
   value.length > acc.length ? value : acc
);

let final = `
///! This module is auto generated.
use super::Opcode;

/// The formatting spacing between opcode and operands for the disassembler output.
pub const DISASSEMBLER_SPACING: usize = ${longestOpcode.length + 1};

${toByte}

${toCode}
`;

fs.writeFileSync(__dirname + "/opcode_impl.rs", final);
