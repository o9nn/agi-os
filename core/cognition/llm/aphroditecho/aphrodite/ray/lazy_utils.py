def is_ray_initialized():
    try:
        import ray
        return ray.is_initialized()
    except ImportError:
        return False
def is_in_ray_actor():
    try:
        import ray
        return ray.is_initialized() and ray.get_runtime_context().get_actor_id() is not None
    except ImportError:
        return False