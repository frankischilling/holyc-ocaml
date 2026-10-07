let classes =
  [
    ("Bool", 1, false);
    ("I8", 1, false);
    ("U8", 1, true);
    ("I16", 2, false);
    ("U16", 2, true);
    ("I32", 4, false);
    ("U32", 4, true);
    ("I64", 8, false);
    ("U64", 8, true);
  ]

let read_matrix =
  List.concat_map
    (fun (original, _, _) ->
      List.map
        (fun (view, width, unsigned) ->
          let expected =
            if unsigned && width < 8 then
              Int64.pred (Int64.shift_left 1L (width * 8))
            else -1L
          in
          ( Printf.sprintf
              "%s a[8]={-1,-1,-1,-1,-1,-1,-1,-1};I64 F(%s *p=a(%s*)){return \
               *p;}F();"
              original view view,
            expected ))
        classes)
    classes

let write_matrix =
  List.concat_map
    (fun (original, _, _) ->
      List.map
        (fun (view, width, _) ->
          let checks =
            List.init 8 (fun byte ->
                Printf.sprintf "if(b[%d]!=%d)return %d;" byte
                  (if byte < width then byte + 1 else 255)
                  (byte + 1))
            |> String.concat ""
          in
          ( Printf.sprintf
              "%s a[8]={-1,-1,-1,-1,-1,-1,-1,-1};I64 F(%s \
               *p=a(%s*)){*p=0x0807060504030201;return 0;}I64 Check(){U8 \
               *b=a(U8*);%s return 42;}F();Check();"
              original view view checks,
            42L ))
        classes)
    classes

let ownership =
  [
    ("I64 A=41;I64 Read(I64 *p=&A){return ++*p;}Read();", 42L);
    ("I64 A=41;I64 Read(I64 *p=&A){return *p;}A=42;Read();", 42L);
    ( "I64 A[2]={17,42};I64 N=0;I64 Read(I64 *p=&A[++N]){return \
       *p;}N=0;Read()+N;",
      42L );
    ("I64 A[2]={17,42};I64 N=0;I64 Unused(I64 *p=&A[++N]){return *p;}N+41;", 42L);
    ( "I64 A[2]={17,42};I64 N=0;I64 Read(I64 *p=&A[++N]){return \
       *p;}Read(&A[0]);N+41;",
      42L );
    ("I64 A[2]={40,2};I64 Read(I64 *p=A){p=&p[1];return *p;}Read()+A[0];", 42L);
    ("I64 A[2]={17,42};I64 Read(I64 *p=&A[2]){return p[-1];}Read();", 42L);
    ( "I64 A=40;I64 Read(I64 *p=&A,I64 n=2){if(n)return Read(p,n-1);return \
       *p+2;}Read();",
      42L );
    ("I64 A=40;I64 Read(I64 *p=&A,...){return *p+argc+argv[0];}Read(,1);", 42L);
    ("I64 A=17;I64 F(I64 *p=&A){return *p;}I64 A=42;F();", 17L);
    ( "I64 A=42;I64 F(I64 *p=&A){return *p;}I64 Old(){return F();}I64 F(I64 \
       *p=&A){return 17;}Old();",
      42L );
    ("I64 A;I64 Set(I64 *p=&A){*p=42;return *p;}Set();", 42L);
    ("U64 A=0;I64 Set(U8 *p=(&A)(U8*)){p[1]=42;return A;}Set();", 10752L);
    ("I64 A=41;I64 F(I64 *p){return ++*p;}I64 (*q)(I64 *p=&A)=&F;q();", 42L);
    ( "I64 A[2]={17,42};I64 N=0;I64 F(I64 *p){return *p;}I64 (*q)(I64 \
       *p=&A[++N])=&F;N=0;q()+N;",
      42L );
    ( "I64 A=42;I64 F(I64 *p){return *p;}I64 (*q)(I64 *p=&A)=&F;I64 Call(I64 \
       (*r)(I64 *p=&A)=q){return r();}Call();",
      42L );
  ]

let strings =
  [
    ("I64 Read(U8 *p=\"AB\"){p[0]=42;return p[0];}Read();", 42L);
    ("I64 Read(U8 *p=\"(\"){return ++*p;}Read();Read();", 42L);
    ("I64 Read(U8 *p=\"prefixAB\"+6){return p[0]-23;}Read();", 42L);
    ("I64 Read(U8 *p=\"ABC\"+3){return *p+42;}Read();", 42L);
    ("I64 Read(U8 *p=\"A\\0B\"){return *p-23;}Read();", 42L);
    ("I64 Read(U32 *p=\"ABCDEFG\"(U32*)){return p[1];}Read();", 0x00474645L);
    ( "I64 F(U8 *p=\"(\"){return ++*p;}I64 G(U8 *p=\"(\"){return *p;}F();G()+2;",
      42L );
    ( "U8 A[3]={65,66,0};I64 Read(U8 *p=A+(\"x\"[0]-120)){return \
       p[0];}A[0]=0;Read();",
      65L );
    ("I64 F(U8 *p){return ++*p;}I64 (*q)(U8 *p=\"(\")=&F;q();q();", 42L);
    ( "extern U0 Print(U8 *fmt,...);I64 F(U8 *p=\"AB\"){Print(\"%s\",p);return \
       42;}F();",
      42L );
  ]

let faults =
  [
    ("I64 A;I64 F(I64 *p=&A){return *p;}F();", "HCIRVM0012");
    ("I64 A[2]={1,2};I64 F(I64 *p=&A[2]){return *p;}F();", "HCIRVM0019");
    ("I64 A[2]={1,2};I64 F(I64 *p=&A[3]){return *p;}F();", "HCIRVM0019");
    ("I64 F(U64 *p=\"AB\"(U64*)){return *p;}F();", "HCIRVM0019");
    ("I64 F(U8 *p=\"A\\0B\"){return p[2];}F();", "HCIRVM0019");
    ("U8 A[2]={65,66};I64 F(U8 *p=A+(\"x\"[0]-120)){return *p;}", "HCIRVM0019");
    ("U8 A[2];I64 F(U8 *p=A+(\"x\"[0]-120)){return *p;}", "HCIRVM0012");
  ]
