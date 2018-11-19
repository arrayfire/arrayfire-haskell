#include "defines.h"

af_err afcl_get_context(cl_context *ctx, const bool retain);
af_err afcl_get_queue(cl_command_queue *queue, const bool retain);
af_err afcl_get_device_id(cl_device_id *id);
af_err afcl_set_device_id(cl_device_id id);
af_err afcl_add_device_context(cl_device_id dev, cl_context ctx, cl_command_queue que);
af_err afcl_set_device_context(cl_device_id dev, cl_context ctx);
af_err afcl_delete_device_context(cl_device_id dev, cl_context ctx);
af_err afcl_get_device_type(afcl_device_type *res);
af_err afcl_get_platform(afcl_platform *res);
