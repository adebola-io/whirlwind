/// A uniform interface for any struct that can iteratively read
/// the instruction stream while advancing an internal counter.
///
/// The counter is post incremented, meaning that it always points
/// to the next instruction byte in the stream.
pub trait BytecodeReader {
    /// Returns the position of the bytecode reader in the stream.
    fn position(&self) -> usize;
    /// Returns the next byte in the stream, advancing the reader by 1.
    fn next_byte(&mut self) -> u8;
    /// Returns the next two bytes in the stream, advancing the reader by 2.
    fn next_two_bytes(&mut self) -> [u8; 2];
    /// Returns the next four bytes in the stream, advancing the reader by 4.
    fn next_four_bytes(&mut self) -> [u8; 4];
    /// Returns the next eight bytes in the stream, advancing the reader by 8.
    fn next_eight_bytes(&mut self) -> [u8; 8];
}

/// A bytecode reader for disassembling bytecode objects.
pub struct DisAsmBytecodeReader<'inst> {
    pub bytes: &'inst [u8],
    /// The instruction pointer.
    pub pc: usize,
}

impl<'inst> From<&'inst [u8]> for DisAsmBytecodeReader<'inst> {
    fn from(value: &'inst [u8]) -> Self {
        DisAsmBytecodeReader {
            bytes: value,
            pc: 0,
        }
    }
}
impl<'inst> BytecodeReader for DisAsmBytecodeReader<'inst> {
    fn position(&self) -> usize {
        self.pc
    }

    #[inline]
    fn next_byte(&mut self) -> u8 {
        let byte = self.bytes[self.pc];
        self.pc += 1;
        return byte;
    }

    #[inline]
    fn next_two_bytes(&mut self) -> [u8; 2] {
        let blob = self.bytes;
        let range = (self.pc)..(self.pc + 2);
        let range_ptr = blob[range].as_ptr();
        let bytes = unsafe { *(range_ptr as *const [u8; 2]) };
        self.pc += 2;
        return bytes;
    }

    #[inline]
    fn next_four_bytes(&mut self) -> [u8; 4] {
        let blob = self.bytes;
        let range = (self.pc)..(self.pc + 4);
        let range_ptr = blob[range].as_ptr();
        let bytes = unsafe { *(range_ptr as *const [u8; 4]) };
        self.pc += 4;
        return bytes;
    }

    #[inline]
    fn next_eight_bytes(&mut self) -> [u8; 8] {
        let blob = self.bytes;
        let range = (self.pc)..(self.pc + 8);
        let range_ptr = blob[range].as_ptr();
        let bytes = unsafe { *(range_ptr as *const [u8; 8]) };
        self.pc += 8;
        return bytes;
    }
}

#[test]
fn test_disassembler_reader() {
    let stream: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
    let mut reader = DisAsmBytecodeReader::from(stream.as_slice());

    assert!(reader.next_byte() == 1);
    assert!(reader.next_two_bytes() == [2, 3]);
    assert!(reader.next_four_bytes() == [4, 5, 6, 7]);
    assert!(reader.next_eight_bytes() == [8, 9, 10, 11, 12, 13, 14, 15])
}
