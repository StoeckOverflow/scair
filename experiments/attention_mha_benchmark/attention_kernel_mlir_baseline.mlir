module {
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
      %outFlat : memref<?xf32>) attributes {llvm.emit_c_interface} {
    %c0 = arith.constant 0 : index
    %c1 = arith.constant 1 : index
    %f0 = arith.constant 0.0 : f32
    %fneg = arith.constant -3.40282347E+38 : f32

    %hidden = arith.muli %heads, %head_dim : index
    %seq_hidden = arith.muli %seq, %hidden : index
    %seq_seq = arith.muli %seq, %seq : index
    %heads_seq_seq = arith.muli %heads, %seq_seq : index
    %head_seq_seq = arith.muli %head_dim, %seq_seq : index

    %Q = memref.reinterpret_cast %Qflat to
      offset: [%c0],
      sizes: [%batch, %seq, %hidden],
      strides: [%seq_hidden, %hidden, %c1]
    : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %K = memref.reinterpret_cast %Kflat to
      offset: [%c0],
      sizes: [%batch, %seq, %hidden],
      strides: [%seq_hidden, %hidden, %c1]
    : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %V = memref.reinterpret_cast %Vflat to
      offset: [%c0],
      sizes: [%batch, %seq, %hidden],
      strides: [%seq_hidden, %hidden, %c1]
    : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %score = memref.reinterpret_cast %scoreFlat to
      offset: [%c0],
      sizes: [%batch, %heads, %seq, %seq],
      strides: [%heads_seq_seq, %seq_seq, %seq, %c1]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %prob = memref.reinterpret_cast %probFlat to
      offset: [%c0],
      sizes: [%batch, %heads, %seq, %seq],
      strides: [%heads_seq_seq, %seq_seq, %seq, %c1]
    : memref<?xf32> to memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>

    %out = memref.reinterpret_cast %outFlat to
      offset: [%c0],
      sizes: [%batch, %seq, %hidden],
      strides: [%seq_hidden, %hidden, %c1]
    : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %tmpOut = memref.reinterpret_cast %tmpOutFlat to
      offset: [%c0],
      sizes: [%batch, %seq, %hidden],
      strides: [%seq_hidden, %hidden, %c1]
    : memref<?xf32> to memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>

    %scale = func.call @bench_inv_sqrt_index(%head_dim) : (index) -> f32

    affine.for %b = 0 to %batch {
      affine.for %h = 0 to %heads {
        %h_base = arith.muli %h, %head_dim : index
        affine.for %i = 0 to %seq {
          affine.for %j = 0 to %seq {
            %sum = affine.for %d = 0 to %head_dim iter_args(%acc = %f0) -> f32 {
              %hd = arith.addi %h_base, %d : index
              %qv = memref.load %Q[%b, %i, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
              %kv = memref.load %K[%b, %j, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
              %prod = arith.mulf %qv, %kv : f32
              %next = arith.addf %acc, %prod : f32
              affine.yield %next : f32
            }
            %scaled = arith.mulf %sum, %scale : f32
            memref.store %scaled, %score[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
          }
        }
      }
    }

    affine.for %b = 0 to %batch {
      affine.for %h = 0 to %heads {
        affine.for %i = 0 to %seq {
          %row_max = affine.for %j = 0 to %seq iter_args(%acc = %fneg) -> f32 {
            %sv = memref.load %score[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
            %next = arith.maximumf %acc, %sv : f32
            affine.yield %next : f32
          }
          %denom = affine.for %j = 0 to %seq iter_args(%acc = %f0) -> f32 {
            %sv = memref.load %score[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
            %shifted = arith.subf %sv, %row_max : f32
            %expv = func.call @bench_expf(%shifted) : (f32) -> f32
            memref.store %expv, %prob[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
            %next = arith.addf %acc, %expv : f32
            affine.yield %next : f32
          }
          affine.for %j = 0 to %seq {
            %expv = memref.load %prob[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
            %norm = arith.divf %expv, %denom : f32
            memref.store %norm, %prob[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
          }
        }
      }
    }

    affine.for %b = 0 to %batch {
      affine.for %i = 0 to %seq {
        affine.for %h = 0 to %heads {
          %h_base = arith.muli %h, %head_dim : index
          affine.for %d = 0 to %head_dim {
            %hd = arith.addi %h_base, %d : index
            %sum = affine.for %j = 0 to %seq iter_args(%acc = %f0) -> f32 {
              %pv = memref.load %prob[%b, %h, %i, %j] : memref<?x?x?x?xf32, strided<[?, ?, ?, ?], offset: ?>>
              %vv = memref.load %V[%b, %j, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
              %prod = arith.mulf %pv, %vv : f32
              %next = arith.addf %acc, %prod : f32
              affine.yield %next : f32
            }
            memref.store %sum, %tmpOut[%b, %i, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
          }
        }
      }
    }

    affine.for %b = 0 to %batch {
      affine.for %i = 0 to %seq {
        %copy_hidden = affine.for %hd = 0 to %hidden iter_args(%seed = %f0) -> f32 {
          %v = memref.load %tmpOut[%b, %i, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
          memref.store %v, %out[%b, %i, %hd] : memref<?x?x?xf32, strided<[?, ?, ?], offset: ?>>
          affine.yield %v : f32
        }
      }
    }

    return
  }
}
