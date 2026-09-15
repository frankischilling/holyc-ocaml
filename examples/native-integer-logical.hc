// Eager logical values, ordinary comparison chains and full-word truth tests.
// The unsigned class forwarded by COM must survive the following chain links.
(
  (2&&4)*10
  +(0||0x8000000000000000)*10
  +(2^^4)*10
  +(0^^0x100)*10
  +((~0x8000000000000000)>0>-1)*100
  +((~0x8000000000000000)>0<-1)*10
  +(1<2<3)
  +(!(0||0))
);
