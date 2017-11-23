def multiPly(a:A,b:A):A={
    if (b==0) 0
    else multiPly(a,b-1)+a
}

//数值过大会造成stack overflow，考虑采用尾递归优化
