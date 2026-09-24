// Run with --conditional-recovery=templeos-permissive.
#endif
I64 RecoveryValue()
{
#ifjit
  return 42;
#else
  return 6*7;
#endif
}
#else
This text is discarded through the matching endif.
#ifjit
Nested conditionals are counted during the raw scan.
#endif
#endif
RecoveryValue;
#ifaot
// Both an active branch and a discarded branch may end at EOF.
