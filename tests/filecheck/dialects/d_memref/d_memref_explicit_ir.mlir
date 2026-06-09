// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | filecheck %s
// RUN: scair-opt %s --allow-unregistered-dialect --verify-diagnostics | scair-opt --allow-unregistered-dialect --verify-diagnostics

builtin.module {
  %m = "arith.constant"() <{value = 4 : index}> : () -> index
  %n = "arith.constant"() <{value = 8 : index}> : () -> index
  %mn = "arith.muli"(%m, %n) : (index, index) -> index
  %z = "arith.constant"() <{value = 0 : index}> : () -> index
  %one = "arith.constant"() <{value = 1 : index}> : () -> index
  %dyn_axis = "test.dynamic_axis"() : () -> index
  %buf = d_memref.alloc : () -> !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
  %flat = d_memref.alloc : () -> !d_memref.memref<[%mn], f32>
  %v = "test.value"() : () -> f32
  d_memref.store %v, %buf[%one, %one] : f32, !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
  %loaded = d_memref.load %buf[%one, %one] : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> f32
  %dim_dyn = d_memref.dim %buf, %dyn_axis : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> index
  %dim_exact = d_memref.dim_exact %buf {axis = 1 : i32} : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> !value<%n>
  %sub = d_memref.subview %buf[%z, %z][%m, %n][%one, %one] : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]> -> !d_memref.memref<[%m, %n], f32>
  %cast = d_memref.cast %sub : !d_memref.memref<[%m, %n], f32> -> !d_memref.memref<[%m, %n], f32>
  %view = d_memref.reinterpret_cast %flat
    : !d_memref.memref<[%mn], f32>
      to !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %one]>
  // d_memref.reinterpret_cast currently verifies element type plus explicit
  // destination layout metadata; it does not prove source/destination element
  // counts equal.
  %metadata_only = d_memref.reinterpret_cast %buf
    : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
      to !d_memref.memref<[%m], f32, offset: %z, strides: [%one]>
  %base, %off, %size0, %stride0, %size1, %stride1 = "d_memref.extract_strided_metadata"(%view)
    : (!d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %one]>)
      -> (!d_memref.memref<[], f32>, index, index, index, index, index)
  "test.keep"(%loaded, %dim_dyn, %dim_exact, %cast, %view, %metadata_only, %base, %off, %size0, %stride0, %size1, %stride1)
    : (f32, index, !value<%n>, !d_memref.memref<[%m, %n], f32>, !d_memref.memref<[%m, %n], f32, offset: %z, strides: [%n, %one]>, !d_memref.memref<[%m], f32, offset: %z, strides: [%one]>, !d_memref.memref<[], f32>, index, index, index, index, index) -> ()
  d_memref.dealloc %buf : !d_memref.memref<[%m, %n], f32, offset: 0, strides: [%n, 1]>
  d_memref.dealloc %flat : !d_memref.memref<[%mn], f32>
}

// CHECK: builtin.module {
// CHECK-NEXT:   %[[M:[0-9]+]] = "arith.constant"() <{value = 4 : index}> : () -> index
// CHECK-NEXT:   %[[N:[0-9]+]] = "arith.constant"() <{value = 8 : index}> : () -> index
// CHECK-NEXT:   %[[MN:[0-9]+]] = "arith.muli"(%[[M]], %[[N]]) {{.*}} : (index, index) -> index
// CHECK-NEXT:   %[[Z:[0-9]+]] = "arith.constant"() <{value = 0 : index}> : () -> index
// CHECK-NEXT:   %[[ONE:[0-9]+]] = "arith.constant"() <{value = 1 : index}> : () -> index
// CHECK-NEXT:   %[[AXIS:[0-9]+]] = "test.dynamic_axis"() : () -> index
// CHECK-NEXT:   %[[BUF:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]>
// CHECK-NEXT:   %[[FLAT:[0-9]+]] = d_memref.alloc : () -> !d_memref.memref<[%[[MN]]], f32>
// CHECK-NEXT:   %[[V:[0-9]+]] = "test.value"() : () -> f32
// CHECK-NEXT:   d_memref.store %[[V]], %[[BUF]][%[[ONE]], %[[ONE]]] : f32, !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]>
// CHECK-NEXT:   %[[LOADED:[0-9]+]] = d_memref.load %[[BUF]][%[[ONE]], %[[ONE]]] : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]> -> f32
// CHECK-NEXT:   %[[DIMDYN:[0-9]+]] = d_memref.dim %[[BUF]], %[[AXIS]] : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]> -> index
// CHECK-NEXT:   %[[DIMEXACT:[0-9]+]] = d_memref.dim_exact %[[BUF]] {axis = 1 : i32} : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]> -> !value<%[[N]]>
// CHECK-NEXT:   %[[SUB:[0-9]+]] = d_memref.subview %[[BUF]][%[[Z]], %[[Z]]][%[[M]], %[[N]]][%[[ONE]], %[[ONE]]] : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]> -> !d_memref.memref<[%[[M]], %[[N]]], f32>
// CHECK-NEXT:   %[[CAST:[0-9]+]] = d_memref.cast %[[SUB]] : !d_memref.memref<[%[[M]], %[[N]]], f32> -> !d_memref.memref<[%[[M]], %[[N]]], f32>
// CHECK-NEXT:   %[[VIEW:[0-9]+]] = d_memref.reinterpret_cast %[[FLAT]]
// CHECK-NEXT:   : !d_memref.memref<[%[[MN]]], f32> to !d_memref.memref<[%[[M]], %[[N]]], f32, offset: %[[Z]], strides: [%[[N]], %[[ONE]]]>
// CHECK-NEXT:   %[[METADATA_ONLY:[0-9]+]] = d_memref.reinterpret_cast %[[BUF]]
// CHECK-NEXT:   : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]> to !d_memref.memref<[%[[M]]], f32, offset: %[[Z]], strides: [%[[ONE]]]>
// CHECK-NEXT:   %[[BASE:[0-9]+]], %[[OFF:[0-9]+]], %[[SIZE0:[0-9]+]], %[[STRIDE0:[0-9]+]], %[[SIZE1:[0-9]+]], %[[STRIDE1:[0-9]+]] = "d_memref.extract_strided_metadata"(%[[VIEW]])
// CHECK-NEXT:   "test.keep"(%[[LOADED]], %[[DIMDYN]], %[[DIMEXACT]], %[[CAST]], %[[VIEW]], %[[METADATA_ONLY]], %[[BASE]], %[[OFF]], %[[SIZE0]], %[[STRIDE0]], %[[SIZE1]], %[[STRIDE1]])
// CHECK:        d_memref.dealloc %[[BUF]] : !d_memref.memref<[%[[M]], %[[N]]], f32, offset: 0, strides: [%[[N]], 1]>
// CHECK-NEXT:   d_memref.dealloc %[[FLAT]] : !d_memref.memref<[%[[MN]]], f32>
// CHECK-NEXT: }
