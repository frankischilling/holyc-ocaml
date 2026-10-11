I64 Pick(I64 value)
{
    I64 result=0;
    switch [value] {
        start:
            result=40;
            case 1: result+=2; break;
            case 2: result+=1; break;
        end:
        break;
        default: result=-1;
    }
    return result;
}

Pick(1)+Pick(2)+Pick(0)-40;
