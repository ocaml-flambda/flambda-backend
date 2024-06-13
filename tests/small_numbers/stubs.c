
#include <caml/float32.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <float.h>
#include <math.h>

int32_t float32_bits_to_int(float f) { return *(int32_t *)&f; }
float float32_of_int(intnat i) { return (float)i; }
float float32_of_int64(int64_t i) { return (float)i; }
float float32_of_float(double d) { return (float)d; }
intnat float32_to_int(float f) { return (intnat)f; }
int64_t float32_to_int64(float f) { return (int64_t)f; }
double float32_to_float(float f) { return (double)f; }
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
float float32_epsilon(value unit) { return FLT_EPSILON; }
float float32_pi(value unit) { return M_PI; }
float float32_abs(float f) { return fabsf(f); }
float float32_neg(float f) { return -f; }
float float32_succ(float f) { return nextafterf(f, INFINITY); }
float float32_pred(float f) { return nextafterf(f, -INFINITY); }
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
float float32_rem(float l, float r) { return fmodf(l, r); }
float float32_pow(float l, float r) { return powf(l, r); }
float float32_fma(float l, float r, float s) { return fmaf(l, r, s); }
value float32_is_nan(float f) { return Val_bool(isnan(f)); }
value float32_is_finite(float f) { return Val_bool(isfinite(f)); }
value float32_is_infinite(float f) { return Val_bool(isinf(f)); }
value float32_is_integer(float f) { return Val_bool(isfinite(f) && truncf(f) == f); }
float float32_sqrt(float f) { return sqrtf(f); }
float float32_cbrt(float f) { return cbrtf(f); }
float float32_exp(float f) { return expf(f); }
float float32_exp2(float f) { return exp2f(f); }
float float32_log(float f) { return logf(f); }
float float32_log10(float f) { return log10f(f); }
float float32_log2(float f) { return log2f(f); }
float float32_expm1(float f) { return expm1f(f); }
float float32_log1p(float f) { return log1pf(f); }
float float32_cos(float f) { return cosf(f); }
float float32_sin(float f) { return sinf(f); }
float float32_tan(float f) { return tanf(f); }
float float32_acos(float f) { return acosf(f); }
float float32_asin(float f) { return asinf(f); }
float float32_atan(float f) { return atanf(f); }
float float32_cosh(float f) { return coshf(f); }
float float32_sinh(float f) { return sinhf(f); }
float float32_tanh(float f) { return tanhf(f); }
float float32_acosh(float f) { return acoshf(f); }
float float32_asinh(float f) { return asinhf(f); }
float float32_atanh(float f) { return atanhf(f); }
float float32_erf(float f) { return erff(f); }
float float32_erfc(float f) { return erfcf(f); }
float float32_trunc(float f) { return truncf(f); }
float float32_round(float f) { return roundf(f); }
float float32_ceil(float f) { return ceilf(f); }
float float32_floor(float f) { return floorf(f); }
float float32_atan2(float l, float r) { return atan2f(l, r); }
float float32_hypot(float l, float r) { return hypotf(l, r); }
float float32_next_after(float l, float r) { return nextafterf(l, r); }
float float32_copy_sign(float l, float r) { return copysignf(l, r); }
value float32_sign_bit(float f) { return Val_bool(signbit(f)); }

value float32_bits_to_int_boxed(value f) { return caml_copy_int32(*(int32_t *)&Float32_val(f)); }
value float32_of_int_boxed(value i) { return caml_copy_float32((float)Long_val(i)); }
value float32_of_float_boxed(value d) { return caml_copy_float32((float)Double_val(d)); }
value float32_to_int_boxed(value f) { return Val_int((intnat)Float32_val(f)); }
value float32_to_float_boxed(value f) { return caml_copy_double((double)Float32_val(f)); }
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
value float32_epsilon_boxed(value unit) { return caml_copy_float32(FLT_EPSILON); }
value float32_pi_boxed(value unit) { return caml_copy_float32(M_PI); }
value float32_abs_boxed(value f) { return caml_copy_float32(fabsf(Float32_val(f))); }
value float32_neg_boxed(value f) { return caml_copy_float32(-Float32_val(f)); }
value float32_succ_boxed(value f) { return caml_copy_float32(nextafterf(Float32_val(f), INFINITY)); }
value float32_pred_boxed(value f) { return caml_copy_float32(nextafterf(Float32_val(f), -INFINITY)); }
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
value float32_rem_boxed(value l, value r) { return caml_copy_float32(fmodf(Float32_val(l), Float32_val(r))); }
value float32_pow_boxed(value l, value r) { return caml_copy_float32(powf(Float32_val(l), Float32_val(r))); }
value float32_fma_boxed(value l, value r, value s) { return caml_copy_float32(fmaf(Float32_val(l), Float32_val(r), Float32_val(s))); }
value float32_is_nan_boxed(value f) { return Val_bool(isnan(Float32_val(f))); }
value float32_is_finite_boxed(value f) { return Val_bool(isfinite(Float32_val(f))); }
value float32_is_infinite_boxed(value f) { return Val_bool(isinf(Float32_val(f))); }
value float32_is_integer_boxed(value f) { return Val_bool(isfinite(Float32_val(f)) && truncf(Float32_val(f)) == Float32_val(f)); }
value float32_sqrt_boxed(value f) { return caml_copy_float32(sqrtf(Float32_val(f))); }
value float32_cbrt_boxed(value f) { return caml_copy_float32(cbrtf(Float32_val(f))); }
value float32_exp_boxed(value f) { return caml_copy_float32(expf(Float32_val(f))); }
value float32_exp2_boxed(value f) { return caml_copy_float32(exp2f(Float32_val(f))); }
value float32_log_boxed(value f) { return caml_copy_float32(logf(Float32_val(f))); }
value float32_log10_boxed(value f) { return caml_copy_float32(log10f(Float32_val(f))); }
value float32_log2_boxed(value f) { return caml_copy_float32(log2f(Float32_val(f))); }
value float32_expm1_boxed(value f) { return caml_copy_float32(expm1f(Float32_val(f))); }
value float32_log1p_boxed(value f) { return caml_copy_float32(log1pf(Float32_val(f))); }
value float32_cos_boxed(value f) { return caml_copy_float32(cosf(Float32_val(f))); }
value float32_sin_boxed(value f) { return caml_copy_float32(sinf(Float32_val(f))); }
value float32_tan_boxed(value f) { return caml_copy_float32(tanf(Float32_val(f))); }
value float32_acos_boxed(value f) { return caml_copy_float32(acosf(Float32_val(f))); }
value float32_asin_boxed(value f) { return caml_copy_float32(asinf(Float32_val(f))); }
value float32_atan_boxed(value f) { return caml_copy_float32(atanf(Float32_val(f))); }
value float32_cosh_boxed(value f) { return caml_copy_float32(coshf(Float32_val(f))); }
value float32_sinh_boxed(value f) { return caml_copy_float32(sinhf(Float32_val(f))); }
value float32_tanh_boxed(value f) { return caml_copy_float32(tanhf(Float32_val(f))); }
value float32_acosh_boxed(value f) { return caml_copy_float32(acoshf(Float32_val(f))); }
value float32_asinh_boxed(value f) { return caml_copy_float32(asinhf(Float32_val(f))); }
value float32_atanh_boxed(value f) { return caml_copy_float32(atanhf(Float32_val(f))); }
value float32_erf_boxed(value f) { return caml_copy_float32(erff(Float32_val(f))); }
value float32_erfc_boxed(value f) { return caml_copy_float32(erfcf(Float32_val(f))); }
value float32_trunc_boxed(value f) { return caml_copy_float32(truncf(Float32_val(f))); }
value float32_round_boxed(value f) { return caml_copy_float32(roundf(Float32_val(f))); }
value float32_ceil_boxed(value f) { return caml_copy_float32(ceilf(Float32_val(f))); }
value float32_floor_boxed(value f) { return caml_copy_float32(floorf(Float32_val(f))); }
value float32_atan2_boxed(value l, value r) { return caml_copy_float32(atan2f(Float32_val(l), Float32_val(r))); }
value float32_hypot_boxed(value l, value r) { return caml_copy_float32(hypotf(Float32_val(l), Float32_val(r))); }
value float32_next_after_boxed(value l, value r) { return caml_copy_float32(nextafterf(Float32_val(l), Float32_val(r))); }
value float32_copy_sign_boxed(value l, value r) { return caml_copy_float32(copysignf(Float32_val(l), Float32_val(r))); }
value float32_sign_bit_boxed(value f) { return Val_bool(signbit(Float32_val(f))); }

value float32_compare_boxed(value l, value r)
{
    float f = Float32_val(l);
    float g = Float32_val(r);
    intnat res =
        (intnat)(f > g) - (intnat)(f < g) + (intnat)(f == f) - (intnat)(g == g);
    return Val_int(res);
}

value float32_min_boxed(value l, value r)
{
    float f = Float32_val(l);
    float g = Float32_val(r);
    if (isnan(f) || isnan(g))
        return caml_copy_float32(NAN);
    if (f == 0.0 && g == 0.0 && (signbit(f) || signbit(g)))
        return caml_copy_float32(-0.0);
    return caml_copy_float32(fminf(f, g));
}

value float32_max_boxed(value l, value r)
{
    float f = Float32_val(l);
    float g = Float32_val(r);
    if (isnan(f) || isnan(g))
        return caml_copy_float32(NAN);
    if (f == 0.0 && g == 0.0 && (!signbit(f) || !signbit(g)))
        return caml_copy_float32(0.0);
    return caml_copy_float32(fmaxf(f, g));
}

value float32_min_num_boxed(value l, value r)
{
    float f = Float32_val(l);
    float g = Float32_val(r);
    if (isnan(f) && isnan(g))
        return caml_copy_float32(NAN);
    if (isnan(f) && !isnan(g))
        return caml_copy_float32(g);
    if (!isnan(f) && isnan(g))
        return caml_copy_float32(f);
    if (f == 0.0 && g == 0.0 && (signbit(f) || signbit(g)))
        return caml_copy_float32(-0.0);
    return caml_copy_float32(fminf(f, g));
}

value float32_max_num_boxed(value l, value r)
{
    float f = Float32_val(l);
    float g = Float32_val(r);
    if (isnan(f) && isnan(g))
        return caml_copy_float32(NAN);
    if (isnan(f) && !isnan(g))
        return caml_copy_float32(g);
    if (!isnan(f) && isnan(g))
        return caml_copy_float32(f);
    if (f == 0.0 && g == 0.0 && (!signbit(f) || !signbit(g)))
        return caml_copy_float32(0.0);
    return caml_copy_float32(fmaxf(f, g));
}

value float32_min_max_boxed(value l, value r)
{
    CAMLparam2(l, r);
    CAMLlocal2(min, max);
    min = float32_min_boxed(l, r);
    max = float32_max_boxed(l, r);
    value res = caml_alloc_small(2, 0);
    Field(res, 0) = min;
    Field(res, 1) = max;
    CAMLreturn(res);
}

value float32_min_max_num_boxed(value l, value r)
{
    CAMLparam2(l, r);
    CAMLlocal2(min, max);
    min = float32_min_num_boxed(l, r);
    max = float32_max_num_boxed(l, r);
    value res = caml_alloc_small(2, 0);
    Field(res, 0) = min;
    Field(res, 1) = max;
    CAMLreturn(res);
}

value float32_frexp_boxed(value f)
{
    CAMLparam1(f);
    CAMLlocal1(mantissa);
    value res;
    int exponent;

    mantissa = caml_copy_float32(frexpf(Float32_val(f), &exponent));
    res = caml_alloc_small(2, 0);
    Field(res, 0) = mantissa;
    Field(res, 1) = Val_int(exponent);
    CAMLreturn(res);
}

value float32_ldexp_boxed(value mant, value exp)
{
    return caml_copy_float32(ldexpf(Float32_val(mant), Int_val(exp)));
}

value float32_modf_boxed(value f)
{
    CAMLparam0();
    CAMLlocal2(quo, rem);
    value res;
    float frem;

    quo = caml_copy_float32(modff(Float32_val(f), &frem));
    rem = caml_copy_float32(frem);
    res = caml_alloc_small(2, 0);
    Field(res, 0) = quo;
    Field(res, 1) = rem;
    CAMLreturn(res);
}

enum
{
    FP_normal,
    FP_subnormal,
    FP_zero,
    FP_infinite,
    FP_nan
};

value float32_classify(float f)
{
    switch (fpclassify(f))
    {
    case FP_NORMAL:
        return Val_int(FP_normal);
    case FP_SUBNORMAL:
        return Val_int(FP_subnormal);
    case FP_ZERO:
        return Val_int(FP_zero);
    case FP_INFINITE:
        return Val_int(FP_infinite);
    case FP_NAN:
        return Val_int(FP_nan);
    default:
        exit(1);
    }
}

value float32_classify_boxed(value f)
{
    return float32_classify(Float32_val(f));
}
