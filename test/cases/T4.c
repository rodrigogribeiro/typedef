int do_math(int arg1, int arg2) {
  return arg2 / arg1;
}

int call_a_func(T call_this) {
    float output = call_this(5,7);
    return output;
}

int main () {
    float final_result = call_a_fun(&do_math);
}
