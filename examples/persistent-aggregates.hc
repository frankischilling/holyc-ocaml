extern U0 Print(U8 *fmt,...);
class Item {
  U8 text[3];
  U16 value;
};
Item items[2];
I64 Run() {
  static Item saved;
  saved.value=40;
  items[1].value=++saved.value;
  items[1].text[0]=65;
  items[1].text[1]=66;
  items[1].text[2]=0;
  Print("%s",items[1].text);
  return ++items[1].value;
}
Run();
