s = "cabaabac"
L,R=0,len(s)-1
while L<R:
    if s[L]!=s[R]:
        break
    c=s[L]
    while L<=R and s[L]==c:
        L+=1
    while L<=R and s[R]==c:
        R-=1
print(R-L+1)