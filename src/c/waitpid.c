
#ifndef __WIN32__
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

void wait_pid( int pid )
{
    pid_t result;
    pid_t p = (pid_t) pid;
    int stat = 0;

tryagain:
    result = waitpid( p, &stat, 0 );
    if ( result == -1 && errno == EINTR ) {
        goto tryagain;
    }
}

void terminate_process( int pid )
{
    kill( (pid_t) pid, SIGTERM );
}
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
void terminate_process( int pid )
{
    HANDLE h = OpenProcess( PROCESS_TERMINATE, FALSE, (DWORD) pid );
    if ( h ) {
        TerminateProcess( h, 1 );
        CloseHandle( h );
    }
}
#endif

