builtin.module {
  func.func private @bench_expf(f32) -> f32
  func.func private @bench_inv_sqrt_index(index) -> f32

  func.func @attention_mha(
    %batch : index,
    %seq : index,
    %heads : index,
    %head_dim : index,
    %Qflat : memref<?xf32>,
    %Kflat : memref<?xf32>,
    %Vflat : memref<?xf32>,
    %scoreFlat : memref<?xf32>,
    %probFlat : memref<?xf32>,
    %tmpOutFlat : memref<?xf32>,
    %outFlat : memref<?xf32>
  ) attributes {llvm.emit_c_interface = true} {
    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %fneg = "arith.constant"() <{value = -3.40282347E+38 : f32}> : () -> f32

    %hidden = "arith.muli"(%heads, %head_dim) : (index, index) -> index
    %seq_hidden = "arith.muli"(%seq, %hidden) : (index, index) -> index
    %seq_seq = "arith.muli"(%seq, %seq) : (index, index) -> index
    %heads_seq_seq = "arith.muli"(%heads, %seq_seq) : (index, index) -> index

    %Q = "memref.reinterpret_cast"(%Qflat, %c0, %batch, %seq, %hidden, %seq_hidden, %hidden, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 3, 3>}>
      : (memref<?xf32>, index, index, index, index, index, index, index)
        -> memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %K = "memref.reinterpret_cast"(%Kflat, %c0, %batch, %seq, %hidden, %seq_hidden, %hidden, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 3, 3>}>
      : (memref<?xf32>, index, index, index, index, index, index, index)
        -> memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %V = "memref.reinterpret_cast"(%Vflat, %c0, %batch, %seq, %hidden, %seq_hidden, %hidden, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 3, 3>}>
      : (memref<?xf32>, index, index, index, index, index, index, index)
        -> memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %score = "memref.reinterpret_cast"(%scoreFlat, %c0, %batch, %heads, %seq, %seq, %heads_seq_seq, %seq_seq, %seq, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %prob = "memref.reinterpret_cast"(%probFlat, %c0, %batch, %heads, %seq, %seq, %heads_seq_seq, %seq_seq, %seq, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 4, 4>}>
      : (memref<?xf32>, index, index, index, index, index, index, index, index, index)
        -> memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %tmpOut = "memref.reinterpret_cast"(%tmpOutFlat, %c0, %batch, %seq, %hidden, %seq_hidden, %hidden, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 3, 3>}>
      : (memref<?xf32>, index, index, index, index, index, index, index)
        -> memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %out = "memref.reinterpret_cast"(%outFlat, %c0, %batch, %seq, %hidden, %seq_hidden, %hidden, %c1)
      <{operandSegmentSizes = array<i32: 1, 1, 3, 3>}>
      : (memref<?xf32>, index, index, index, index, index, index, index)
        -> memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %scale = "func.call"(%head_dim) <{"callee" = @bench_inv_sqrt_index}> : (index) -> f32

    affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
        %h_base = "arith.muli"(%h, %head_dim) : (index, index) -> index
        affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
          affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
            %sum = affine.for %d = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%head_dim) step 1 : index iter_args(%acc = %f0 : f32) {
              %hd = "arith.addi"(%h_base, %d) : (index, index) -> index
              %qv = "memref.load"(%Q, %b, %i, %hd) : (memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> f32
              %kv = "memref.load"(%K, %b, %j, %hd) : (memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> f32
              %prod = "arith.mulf"(%qv, %kv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %prod) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              affine.yield %next : f32
            }
            %scaled = "arith.mulf"(%sum, %scale) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            "memref.store"(%scaled, %score, %b, %h, %i, %j) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
          }
        }
      }
    }

    affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
        affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
          %row_max = affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %fneg : f32) {
            %sv = "memref.load"(%score, %b, %h, %i, %j) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
            %next = "arith.maximumf"(%acc, %sv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            affine.yield %next : f32
          }
          %denom = affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %f0 : f32) {
            %sv = "memref.load"(%score, %b, %h, %i, %j) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
            %shifted = "arith.subf"(%sv, %row_max) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            %expv = "func.call"(%shifted) <{"callee" = @bench_expf}> : (f32) -> f32
            "memref.store"(%expv, %prob, %b, %h, %i, %j) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
            %next = "arith.addf"(%acc, %expv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            affine.yield %next : f32
          }
          affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
            %expv = "memref.load"(%prob, %b, %h, %i, %j) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
            %norm = "arith.divf"(%expv, %denom) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            "memref.store"(%norm, %prob, %b, %h, %i, %j) : (f32, memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> ()
          }
        }
      }
    }

    affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
        affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
          %h_base = "arith.muli"(%h, %head_dim) : (index, index) -> index
          affine.for %d = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%head_dim) step 1 : index {
            %hd = "arith.addi"(%h_base, %d) : (index, index) -> index
            %sum = affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %f0 : f32) {
              %pv = "memref.load"(%prob, %b, %h, %i, %j) : (memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>, index, index, index, index) -> f32
              %vv = "memref.load"(%V, %b, %j, %hd) : (memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> f32
              %prod = "arith.mulf"(%pv, %vv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %prod) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              affine.yield %next : f32
            }
            "memref.store"(%sum, %tmpOut, %b, %i, %hd) : (f32, memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> ()
          }
        }
        %copy_hidden = affine.for %hd = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%hidden) step 1 : index iter_args(%seed = %f0 : f32) {
          %v = "memref.load"(%tmpOut, %b, %i, %hd) : (memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> f32
          "memref.store"(%v, %out, %b, %i, %hd) : (f32, memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>, index, index, index) -> ()
          affine.yield %v : f32
        }
      }
    }

    "func.return"() : () -> ()
  }
}
