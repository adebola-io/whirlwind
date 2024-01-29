(module
   (import "io" "print" (func $log (param i32 i32)))
   (import "mem" "memory" (memory 1))
   (data (i32.const 0) "Hello world")
   (func (export "writeHi")
        i32.const 0
        i32.const 11
        call $log
    )
)
