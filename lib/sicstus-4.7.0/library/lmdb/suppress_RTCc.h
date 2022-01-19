// Use this as cl.exe /FI suppress_RTCc.h ... some_file_that_triggers_RTC.c

// The LMDB file mdb.c triggers the /RTCc runtime checks ("a value is
// assigned to a smaller data type that results in a data loss."). We
// always have these enabled in debug builds and there is no option
// for turning it off from the command line, e.g. /RTCc- does not
// work.

#pragma runtime_checks( "c", off )
