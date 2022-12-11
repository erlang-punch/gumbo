/*
 *
 */
#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include "gumbo_erlang.h"

typedef struct {
  ErlDrvPort port;
} data;

static ErlDrvData
gumbo_driver_start(ErlDrvPort port, char *buff) {
  data* d = (data*)driver_alloc(sizeof(data));
  d->port = port;
  return (ErlDrvData)d;
}

static void
gumbo_driver_stop(ErlDrvData handle) {
  driver_free((char*)handle);
}

static void
gumbo_driver_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen) {
  data* d = (data*)handle;
  char message_size = sizeof(int);
  char message[message_size];
  (void)bzero(message, message_size);

  int valid = gumbo_html_validation(buff, bufflen);
  (void)strncpy(message, (char *)&valid, message_size);
  driver_output(d->port, message, message_size);
}

ErlDrvEntry gumbo_driver = {
  NULL,                /* F_PTR init, called when driver is loaded */
  gumbo_driver_start,  /* L_PTR start, called when port is opened */
  gumbo_driver_stop,   /* F_PTR stop, called when port is closed */
  gumbo_driver_output, /* F_PTR output, called when erlang has sent */
  NULL,                /* F_PTR ready_input, called when input descriptor ready */
  NULL,                /* F_PTR ready_output, called when output descriptor ready */
  "gumbo_driver",      /* char *driver_name, the argument to open_port */
  NULL,                /* F_PTR finish, called when unloaded */
  NULL,                /* void *handle, Reserved by VM */
  NULL,                /* F_PTR control, port_command callback */
  NULL,                /* F_PTR timeout, reserved */
  NULL,                /* F_PTR outputv, reserved */
  NULL,                /* F_PTR ready_async, only for async drivers */
  NULL,                /* F_PTR flush, called when port is about
                          to be closed, but there is data in driver
                          queue */
  NULL,                 /* F_PTR call, much like control, sync call
                           to driver */
  NULL,                       /* unused */
  ERL_DRV_EXTENDED_MARKER,    /* int extended marker, Should always be
                                 set to indicate driver versioning */
  ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be
                                     set to this value */
  ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be
                                     set to this value */
  0,                          /* int driver_flags, see documentation */
  NULL,                       /* void *handle2, reserved for VM use */
  NULL,                       /* F_PTR process_exit, called when a
                                 monitored process dies */
  NULL                        /* F_PTR stop_select, called to close an
                                     event object */
};

DRIVER_INIT(gumbo_driver) /* must match name in driver_entry */
{
  return &gumbo_driver;
}
