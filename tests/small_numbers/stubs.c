
#include <caml/float32.h>
#include <caml/alloc.h>
#include <float.h>
#include <math.h>

int32_t float32_bits_to_int(float f) { return *(int32_t*)&f; }
float float32_zero(value unit) { return 0.0f; }
float float32_neg_zero(value unit) { return -0.0f; }
float float32_one(value unit) { return 1.0f; }
float float32_neg_one(value unit) { return -1.0f; }
float float32_nan(value unit) { return NAN; }
float float32_nan2(value unit) { return -NAN; }
float float32_neg_infinity(value unit) { return -INFINITY; }
float float32_infinity(value unit) { return INFINITY; }
float float32_maxv(value unit) { return FLT_MAX; }
float float32_minv(value unit) { return FLT_MIN; }
float float32_abs(float f) { return fabsf(f); }
float float32_neg(float f) { return -f; }
value float32_eq(float l, float r) { return Val_bool(l == r); }
value float32_lt(float l, float r) { return Val_bool(l < r); }
value float32_le(float l, float r) { return Val_bool(l <= r); }
value float32_gt(float l, float r) { return Val_bool(l > r); }
value float32_ge(float l, float r) { return Val_bool(l >= r); }
value float32_ne(float l, float r) { return Val_bool(l != r); }
value float32_nle(float l, float r) { return Val_bool(!(l <= r)); }
value float32_nlt(float l, float r) { return Val_bool(!(l < r)); }
value float32_nge(float l, float r) { return Val_bool(!(l >= r)); }
value float32_ngt(float l, float r) { return Val_bool(!(l > r)); }
value float32_ord(float l, float r) { return Val_bool(!(isnan(l) || isnan(r))); }
value float32_uord(float l, float r) { return Val_bool(isnan(l) || isnan(r)); }
float float32_add(float l, float r) { return l + r; }
float float32_sub(float l, float r) { return l - r; }
float float32_mul(float l, float r) { return l * r; }
float float32_div(float l, float r) { return l / r; }
value float32_is_nan(float f) { return Val_bool(isnan(f)); }

value float32_bits_to_int_boxed(value f) { return caml_copy_int32(*(int32_t*)&Float32_val(f)); }
value float32_zero_boxed(value unit) { return caml_copy_float32(0.0f); }
value float32_neg_zero_boxed(value unit) { return caml_copy_float32(-0.0f); }
value float32_one_boxed(value unit) { return caml_copy_float32(1.0f); }
value float32_neg_one_boxed(value unit) { return caml_copy_float32(-1.0f); }
value float32_nan_boxed(value unit) { return caml_copy_float32(NAN); }
value float32_nan2_boxed(value unit) { return caml_copy_float32(-NAN); }
value float32_neg_infinity_boxed(value unit) { return caml_copy_float32(-INFINITY); }
value float32_infinity_boxed(value unit) { return caml_copy_float32(INFINITY); }
value float32_maxv_boxed(value unit) { return caml_copy_float32(FLT_MAX); }
value float32_minv_boxed(value unit) { return caml_copy_float32(FLT_MIN); }
value float32_abs_boxed(value f) { return caml_copy_float32(fabsf(Float32_val(f))); }
value float32_neg_boxed(value f) { return caml_copy_float32(-Float32_val(f)); }
value float32_eq_boxed(value l, value r) { return Val_bool(Float32_val(l) == Float32_val(r)); }
value float32_lt_boxed(value l, value r) { return Val_bool(Float32_val(l) < Float32_val(r)); }
value float32_le_boxed(value l, value r) { return Val_bool(Float32_val(l) <= Float32_val(r)); }
value float32_gt_boxed(value l, value r) { return Val_bool(Float32_val(l) > Float32_val(r)); }
value float32_ge_boxed(value l, value r) { return Val_bool(Float32_val(l) >= Float32_val(r)); }
value float32_ne_boxed(value l, value r) { return Val_bool(Float32_val(l) != Float32_val(r)); }
value float32_nle_boxed(value l, value r) { return Val_bool(!(Float32_val(l) <= Float32_val(r))); }
value float32_nlt_boxed(value l, value r) { return Val_bool(!(Float32_val(l) < Float32_val(r))); }
value float32_nge_boxed(value l, value r) { return Val_bool(!(Float32_val(l) >= Float32_val(r))); }
value float32_ngt_boxed(value l, value r) { return Val_bool(!(Float32_val(l) > Float32_val(r))); }
value float32_ord_boxed(value l, value r) { return Val_bool(!(isnan(Float32_val(l)) || isnan(Float32_val(r)))); }
value float32_uord_boxed(value l, value r) { return Val_bool(isnan(Float32_val(l)) || isnan(Float32_val(r))); }
value float32_add_boxed(value l, value r) { return caml_copy_float32(Float32_val(l) + Float32_val(r)); }
value float32_sub_boxed(value l, value r) { return caml_copy_float32(Float32_val(l) - Float32_val(r)); }
value float32_mul_boxed(value l, value r) { return caml_copy_float32(Float32_val(l) * Float32_val(r)); }
value float32_div_boxed(value l, value r) { return caml_copy_float32(Float32_val(l) / Float32_val(r)); }
value float32_is_nan_boxed(value f) { return Val_bool(isnan(Float32_val(f))); }
