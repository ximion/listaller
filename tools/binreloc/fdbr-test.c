#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>

int main()
{
    int fd = open("/proc/self/fd/200/binreloc/fdbr.c", O_RDONLY);
    int num = 0;
    char buf[100];
    
    memset(buf, 0, sizeof(buf));

    while ((num = read(fd, buf, sizeof(buf) - 2)) > 0)
    {
        buf[num] = '\0';
        printf("%s", buf);
    }
    
}
