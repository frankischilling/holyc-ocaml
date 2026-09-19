I64 Pick(I64 value)
{
    switch (value) {
        case 17...65551: return 42;
        default: return -1;
    }
}

if (Pick(16)!=-1 || Pick(17)!=42 || Pick(40000)!=42 ||
    Pick(65551)!=42 || Pick(65552)!=-1 || Pick(-1)!=-1)
    0;
else
    42;
