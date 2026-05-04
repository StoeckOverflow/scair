builtin.module {
  func.func private @bench_expf(f32) -> f32
  func.func private @bench_inv_sqrt_index(index) -> f32

  func.func @attention_mha(
    %batch_nat : !dtensor.nat,
    %seq_nat : !dtensor.nat,
    %heads_nat : !dtensor.nat,
    %head_dim_nat : !dtensor.nat,
    %Qflat : !d_memref.memref<[], f32>,
    %Kflat : !d_memref.memref<[], f32>,
    %Vflat : !d_memref.memref<[], f32>,
    %scoreFlat : !d_memref.memref<[], f32>,
    %probFlat : !d_memref.memref<[], f32>,
    %tmpOutFlat : !d_memref.memref<[], f32>,
    %outFlat : !d_memref.memref<[], f32>
  ) attributes {scair.emit_bare_interface = true} {
    %hidden_nat = "dtensor.nat.mul"(%heads_nat, %head_dim_nat) : (!dtensor.nat, !dtensor.nat) -> !dtensor.nat

    %c0 = "arith.constant"() <{value = 0 : index}> : () -> index
    %c1 = "arith.constant"() <{value = 1 : index}> : () -> index
    %f0 = "arith.constant"() <{value = 0.0 : f32}> : () -> f32
    %fneg = "arith.constant"() <{value = -3.40282347E+38 : f32}> : () -> f32

    %batch = "dtensor.shape.to_index"(%batch_nat) : (!dtensor.nat) -> index
    %seq = "dtensor.shape.to_index"(%seq_nat) : (!dtensor.nat) -> index
    %heads = "dtensor.shape.to_index"(%heads_nat) : (!dtensor.nat) -> index
    %head_dim = "dtensor.shape.to_index"(%head_dim_nat) : (!dtensor.nat) -> index
    %hidden = "dtensor.shape.to_index"(%hidden_nat) : (!dtensor.nat) -> index
    %seq_hidden = "arith.muli"(%seq, %hidden) : (index, index) -> index
    %seq_seq = "arith.muli"(%seq, %seq) : (index, index) -> index
    %heads_seq_seq = "arith.muli"(%heads, %seq_seq) : (index, index) -> index

    %Q = d_memref.reinterpret_cast %Qflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32,
             offset: 0, strides: [%seq_hidden, %hidden, %c1]>

    %K = d_memref.reinterpret_cast %Kflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32,
             offset: 0, strides: [%seq_hidden, %hidden, %c1]>

    %V = d_memref.reinterpret_cast %Vflat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32,
             offset: 0, strides: [%seq_hidden, %hidden, %c1]>

    %score = d_memref.reinterpret_cast %scoreFlat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32,
             offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]>

    %prob = d_memref.reinterpret_cast %probFlat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32,
             offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]>

    %out = d_memref.reinterpret_cast %outFlat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32,
             offset: 0, strides: [%seq_hidden, %hidden, %c1]>

    %tmpOut = d_memref.reinterpret_cast %tmpOutFlat
      : !d_memref.memref<[], f32>
        to !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32,
             offset: 0, strides: [%seq_hidden, %hidden, %c1]>

    %scale = "func.call"(%head_dim) <{"callee" = @bench_inv_sqrt_index}> : (index) -> f32

    d_affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      d_affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
        %h_base = "arith.muli"(%h, %head_dim) : (index, index) -> index
        d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
          d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
            %sum = d_affine.for %d = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%head_dim) step 1 : index iter_args(%acc = %f0 : f32) {
              %hd = "arith.addi"(%h_base, %d) : (index, index) -> index
              %qv = d_memref.load %Q[%b, %i, %hd] : !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]> -> f32
              %kv = d_memref.load %K[%b, %j, %hd] : !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]> -> f32
              %prod = "arith.mulf"(%qv, %kv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %prod) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            %scaled = "arith.mulf"(%sum, %scale) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_memref.store %scaled, %score[%b, %h, %i, %j] : f32, !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }

    d_affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      d_affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
        d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
          %row_max = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %fneg : f32) {
            %sv = d_memref.load %score[%b, %h, %i, %j] : !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]> -> f32
            %next = "arith.maximumf"(%acc, %sv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_affine.yield %next : (f32)
          }
          %denom = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %f0 : f32) {
            %sv = d_memref.load %score[%b, %h, %i, %j] : !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]> -> f32
            %shifted = "arith.subf"(%sv, %row_max) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            %expv = "func.call"(%shifted) <{"callee" = @bench_expf}> : (f32) -> f32
            d_memref.store %expv, %prob[%b, %h, %i, %j] : f32, !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]>
            %next = "arith.addf"(%acc, %expv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_affine.yield %next : (f32)
          }
          d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
            %expv = d_memref.load %prob[%b, %h, %i, %j] : !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]> -> f32
            %norm = "arith.divf"(%expv, %denom) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
            d_memref.store %norm, %prob[%b, %h, %i, %j] : f32, !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]>
            d_affine.yield
          }
          d_affine.yield
        }
        d_affine.yield
      }
      d_affine.yield
    }

    d_affine.for %b = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%batch) step 1 : index {
      d_affine.for %i = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index {
        d_affine.for %h = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%heads) step 1 : index {
          %h_base = "arith.muli"(%h, %head_dim) : (index, index) -> index
          d_affine.for %d = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%head_dim) step 1 : index {
            %hd = "arith.addi"(%h_base, %d) : (index, index) -> index
            %sum = d_affine.for %j = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%seq) step 1 : index iter_args(%acc = %f0 : f32) {
              %pv = d_memref.load %prob[%b, %h, %i, %j] : !d_memref.memref<[%batch_nat, %heads_nat, %seq_nat, %seq_nat], f32, offset: 0, strides: [%heads_seq_seq, %seq_seq, %seq, %c1]> -> f32
              %vv = d_memref.load %V[%b, %j, %hd] : !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]> -> f32
              %prod = "arith.mulf"(%pv, %vv) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              %next = "arith.addf"(%acc, %prod) <{fastmath = #arith.fastmath<none>}> : (f32, f32) -> f32
              d_affine.yield %next : (f32)
            }
            d_memref.store %sum, %tmpOut[%b, %i, %hd] : f32, !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]>
            d_affine.yield
          }
          d_affine.yield
        }
        %copy_hidden = d_affine.for %hd = affine_map<(d0) -> (d0)>(%c0) to affine_map<(d0) -> (d0)>(%hidden) step 1 : index iter_args(%acc_seed = %f0 : f32) {
          %v = d_memref.load %tmpOut[%b, %i, %hd] : !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]> -> f32
          d_memref.store %v, %out[%b, %i, %hd] : f32, !d_memref.memref<[%batch_nat, %seq_nat, %hidden_nat], f32, offset: 0, strides: [%seq_hidden, %hidden, %c1]>
          d_affine.yield %v : (f32)
        }
        d_affine.yield
      }
      d_affine.yield
    }

    "func.return"() : () -> ()
  }
}
