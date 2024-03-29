(module
  (import "env" "memory" (memory 1))  ; Import standard memory

  (type $mem_t (struct (field $ptr i32) (field $size i32)))  ; Type representing a block of memory

  (global $heap_base i32 (i32.const 1024))  ; Start of our 'heap' (after initial memory)
  (global $last_alloc $mem_t (struct.new $mem_t (i32.const 1024) (i32.const 0))) ;  Track last allocation

  (func $malloc (param $size i32) (result $mem_t)
    ;; Find a suitable free block
    local $current_block $mem_t
    local $end i32
    (block $search_loop
      (loop
        (set $current_block (struct.get $last_alloc $mem_t 0))  ; Get pointer to last allocation
        (set $end (i32.add $current_block (struct.get $last_alloc $mem_t 1)))  ; Calculate block end

        ;; Check if block is free and large enough
        (if (and (i32.eq (struct.get $last_alloc $mem_t 1) (i32.const 0)) (i32.ge_u $size (struct.get $last_alloc $mem_t 1)))
          (then
            ;; Found it! Update allocation status and return.
            (struct.set $last_alloc $mem_t 1 $size)  ; Mark block as allocated
            (return (struct.new $mem_t $current_block $size))
         )
        )

        ;; Is there more memory we can access?
        (if (i32.lt_s (i32.add $end (i32.const 65536)) (memory.size)) ; Check against max 64KB pages
          (then 
            (if (i32.eq (i32.grow_memory (i32.const 1)) (i32.const -1)) ; Attempt to grow by 1 page
              (then
                (return (struct.new $mem_t (i32.const 0) (i32.const 0))) ; Allocation failed 
              )
            )
            (set $last_alloc $mem_t (struct.new $mem_t $end (i32.const 0))) ; Set last allocation to the new space
            (br $search_loop)  ; Try again        
          )
          (else
            (return (struct.new $mem_t (i32.const 0) (i32.const 0))) ; Allocation failed - out of memory
          )
        )
      )
    ) 
  )
) 
