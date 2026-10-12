extern U0 Print(U8 *fmt,...);
class Item{U8 text[3];U16 value;};
Item items[2]={{"CD",20},{"AB",40}};
I64 F(){
  static Item saved={"EF",2};
  Print("%s",items[1].text);
  return items[1].value+saved.value;
}
F();
