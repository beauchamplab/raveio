# Used by RAVE to serialize/unserialize objects
import pickle

def rave_serialize(x, path, name):
    '''
    Serialize RAVE pipeline objects and save them to files
    
    @param x: Python object to serialize
    @param path: absolute path where the serialized data is to be saved to.
        The file path might or might not exist, but the parent directory is 
        always guaranteed.
    @param name: the target export name (variable name for `x`), mainly used
        for branching if the object needs special treatment (such as saving
        as text format).
    
    @return: the saved path, which will be passed to `rave_unserialize` during
        unserialization.
    '''
    with open(path, 'wb') as con:
        # Pickle the 'data' dictionary using the highest protocol available.
        pickle.dump(x, con)
    return path

def rave_unserialize(path, name):
    '''
    Read and unserialize RAVE Pipeline objects from file(s)
    
    @param path: path to the saved objects (see `rave_serialize`)
    @param name: name of the pipeline target (variable name)
    
    @return: A python object or `None` if the object cannot be unserialized
    '''
    with open(path, 'rb') as con:
        data = pickle.load(con)
    return data


