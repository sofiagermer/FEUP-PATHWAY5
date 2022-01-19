#ifndef SPIO_SOCKET_H_INCLUDED
#define SPIO_SOCKET_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#if SPIO_UNIX
#include <sys/socket.h>         /* POSIX */
#endif  /* SPIO_UNIX */

#if SPIO_WIN32
#ifndef STRICT
#define STRICT 1
#endif
/* [PM] 4.0 Always include winsock2 instead of windows to prevent winsock.h from making subsequent inclusion of winsock2.h impossible */
#include <winsock2.h>
#include <Ws2tcpip.h>           /* getaddrinfo */
#endif  /* SPIO_WIN32 */

#if SPIO_UNIX
typedef int spio_t_socket;
#define SPIO_INVALID_SOCKET -1
#define SPIO_SOCKET_ERROR -1

#elif SPIO_WIN32

typedef SOCKET spio_t_socket;
#define SPIO_INVALID_SOCKET INVALID_SOCKET
#define SPIO_SOCKET_ERROR SOCKET_ERROR

#endif  /* SPIO_UNIX */

extern spio_t_error_code spio_socket_accept(spio_t_socket listener, char **pnode_addr, char **pserv_addr, spio_t_socket *accepted_socket, spio_t_bits options);

#define SPIO_SOCKET_CONNECT_OPTION_ADDRESS_FAMILY_UNIX SPIO_BIT(0)
extern spio_t_error_code spio_socket_connect(char const *nodename, char const *servname, spio_t_socket *psocket, spio_t_bits options);

#define SPIO_SOCKET_CLOSE_OPTION_ABORTIVE SPIO_BIT(0)
#if 0
 #define SPIO_SOCKET_CLOSE_OPTION_NOT_CONNECTED SPIO_NEXT_BIT(SPIO_SOCKET_CLOSE_OPTION_ABORTIVE) /* disables shutdown  */
#endif
extern spio_t_error_code spio_socket_close(spio_t_socket sock, spio_t_bits options);

#define SPIO_SOCKET_SHUTDOWN_OPTION_READ SPIO_BIT(0)
#define SPIO_SOCKET_SHUTDOWN_OPTION_WRITE SPIO_NEXT_BIT(SPIO_SOCKET_SHUTDOWN_OPTION_READ)
#define SPIO_SOCKET_SHUTDOWN_OPTION_THREAD_SAFE SPIO_NEXT_BIT(SPIO_SOCKET_SHUTDOWN_OPTION_WRITE) /* not called from main thread */
extern spio_t_error_code spio_socket_shutdown(spio_t_socket, spio_t_bits options);

#define SPIO_SOCKET_LISTENER_OPTION_LOOPBACK         SPIO_BIT(0)
#define SPIO_SOCKET_LISTENER_OPTION_NUMERIC_NODENAME SPIO_NEXT_BIT(SPIO_SOCKET_LISTENER_OPTION_LOOPBACK)
#define SPIO_SOCKET_LISTENER_OPTION_NUMERIC_SERVNAME SPIO_NEXT_BIT(SPIO_SOCKET_LISTENER_OPTION_NUMERIC_NODENAME)
#define SPIO_SOCKET_LISTENER_OPTION_REUSEADDR        SPIO_NEXT_BIT(SPIO_SOCKET_LISTENER_OPTION_NUMERIC_SERVNAME)
#define SPIO_SOCKET_LISTENER_OPTION_ADDRESS_FAMILY_UNIX SPIO_NEXT_BIT(SPIO_SOCKET_LISTENER_OPTION_REUSEADDR)
extern spio_t_error_code spio_socket_listener(char const *nodename, char *servname_buf, size_t servname_buf_size, spio_t_socket *psocket, spio_t_bits options);

#if SPIO_WIN32
extern spio_t_error_code spio_socket_win32_wait(spio_t_socket sock, int lNetworkEventBit);
#endif  /* SPIO_WIN32 */

extern spio_t_error_code spio_hostname(char **pname, spio_t_bits options);

#if SPIO_WIN32
#define SPIO_SOCKET_EVENT_WIN32_OPTION_FREE SPIO_BIT(0)
#define SPIO_SOCKET_EVENT_WIN32_OPTION_ACCEPT SPIO_NEXT_BIT(SPIO_SOCKET_EVENT_WIN32_OPTION_FREE)
extern spio_t_error_code spio_socket_event_win32(spio_t_socket sock, HANDLE *phEvent, spio_t_bits options);
#endif  /* SPIO_WIN32 */

extern spio_t_error_code spio_init_socket(spio_t_bits options);

/*
  SPIO_S_TRUE (SPIO_S_FALSE) if a and b have the same (different or incomparable) address (e.g. for comparing getpeername and getsockname addresses).
*/
extern spio_t_error_code spio_socket_addr_equal(struct sockaddr const *a, socklen_t a_len, struct sockaddr const *b, socklen_t b_len);


#endif  /* SPIO_SOCKET_H_INCLUDED */
