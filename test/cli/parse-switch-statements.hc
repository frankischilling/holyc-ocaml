I64 value;

switch (value) {
  case:
    ;
  case 1:
    value++;
  case 4...7:
    value--;
  start:
    "[";
    default:
      break;
  end:
    "] ";
    break;
}

switch [value] {
  case 0:
    ;
}
