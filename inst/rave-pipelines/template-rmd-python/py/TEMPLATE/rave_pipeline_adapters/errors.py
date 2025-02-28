class RAVERuntimeException(object):
    '''
    A basic RAVE runtime error class that prints dev-friendly error information
    '''
    original_exception = None
    def __init__(self, e):
        if isinstance(e, Exception):
            self.original_exception = e
        elif isinstance(e, str):
            self.original_exception = Exception(e)
        else:
            self.original_exception = Exception('Unknown error')
    def __str__(self):
        return '{}: {}'.format(type(self.original_exception).__name__, self.original_exception)


def rave_target(func):
    '''
    A RAVE runtime decorator to make sure the Python function is properly 
    try-catch'ed. Instead of raising errors, a pipeline target should either
    return an object or an error instance. Using `@rave_target` decorator
    simplifies this error handling.
    '''
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            return RAVERuntimeException(e)
    return wrapper
