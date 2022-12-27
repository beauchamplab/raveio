import os
import numpy
import nipy
from nipy.core.api import AffineTransform
import nipy.algorithms
import nipy.algorithms.resample
import nipy.algorithms.registration.histogram_registration as registration

def coregistrate_image(
  source = 'CT.nii', target = "MR.nii", 
  clean_source = True, inverse_target = True, precenter_source = True,
  smooth=0., reg_type='rigid', interp='pv', similarity = "crl1",
  optimizer='powell', xtol=0.0001, ftol=0.0001):
  '''Coregistrate source image into target image
  Parameters
  ----------
  source : str
      absolute path to CT image
  target : str
      absolute path to MR image
  clean_source : bool
      whether to replace negative values from the CT image with zeros
  inverse_target : bool
      whether to inverse the MRI color intensity
  precenter_source : bool
      whether to re-center the CT sform such that the origin of CT-RAS
      is at the center of the volume. This is useful when CT has 
      large translation
  smooth : float
      a smoothing parameter in mm
  reg_type : {'rigid', 'affine'}
      registration type
  interp : {'pv','tri'} 
      changes the interpolation method for resampling
  xtol : float
      tolerance parameter for function minimization
  ftol : float
      tolerance parameter for function minimization
  similarity : str
      cost function of coregistration optimizer
  '''
  
  target_file = os.path.abspath(path=target)
  source_file = os.path.abspath(path=source)
  
  # files will be saved to the directory where CT is stored
  source_directory = os.path.dirname(source_file)
  
  print("Computing registration from source image to reference image")
  ctimg = nipy.load_image(filename=source_file)
  mrimg = nipy.load_image(filename=target_file)
  ct_cmap = ctimg.coordmap  
  mr_cmap = mrimg.coordmap
  
  # remove negative values from CT as they are less likely to be bones
  if clean_source:
    # ctimg._data[ctimg._data < 0] = 0
    ctdata = ctimg.get_data()
    ctdata[ ctdata < 0 ] = 0
  
  if inverse_target:
    # mrimg._data = numpy.max(mrimg._data) - mrimg._data
    mrdata = mrimg.get_data()
    mrdata[...] = numpy.max(mrdata) - mrdata
  
  # Sometimes CT has large translation in its sform, the registration
  # will fail
  ct_sform = ct_cmap.affine.copy()
  
  # this step places the CT to its volume center
  if precenter_source:
    ct_sform2 = ct_cmap.affine
    ct_sform2[0, 3] = 0
    ct_sform2[1, 3] = 0
    ct_sform2[2, 3] = 0
    ct_cmap_translate = numpy.matmul(ct_sform2, numpy.array([*ctimg.shape,1])) / 2.
    ct_sform2[0, 3] = -ct_cmap_translate[0]
    ct_sform2[1, 3] = -ct_cmap_translate[1]
    ct_sform2[2, 3] = -ct_cmap_translate[2]
  
  ct_sform2 = ct_cmap.affine.copy()
  
  # Compute registration
  ct_to_mri_reg = registration.HistogramRegistration(
    from_img=ctimg, to_img=mrimg, similarity=similarity, smooth=smooth, interp=interp)
  aff = ct_to_mri_reg.optimize(reg_type, optimizer=optimizer, xtol=xtol, ftol=ftol).as_affine()   
  
  # reverse giant move
  ct_cmap.affine[...] = ct_sform
  
  # aff: CT-RAS (recentered to MRI-RAS)
  # ct_sform2: IJK to recentered CT-RAS
  # ct_sform: IJK to underlying CT-RAS
  
  # CT IJK to MRI RAS
  ct_ijk_to_mr_ras = numpy.matmul( aff, ct_sform2 )
  aff2 = numpy.matmul( ct_ijk_to_mr_ras, numpy.linalg.inv(ct_sform) )
  
  print("Saving the transform matrices")
  numpy.savetxt(
    os.path.join(source_directory, 'CT_IJK_to_MR_RAS.txt'),
    ct_ijk_to_mr_ras, delimiter = "\t", fmt = "%.10f"
  )
  numpy.savetxt(
    os.path.join(source_directory, 'CT_RAS_to_MR_RAS.txt'),
    aff2, delimiter = "\t", fmt = "%.10f"
  )
  
  ct_to_mri = AffineTransform(ct_cmap.function_range, mr_cmap.function_range, aff2)  
  reg_CT = nipy.algorithms.resample.resample(ctimg, mr_cmap, ct_to_mri.inverse(), mrimg.shape)    
  
  outfile = os.path.join(source_directory, 'ct_in_t1.nii')
  print("Saving registration as ct_in_t1.nii")
  nipy.save_image(reg_CT, outfile, dtype_from="data")
  
  return outfile


if __name__ == "__main__":
  import os
  
  # run within RAVE
  source = """{{ ct_path }}"""
  target = """{{ mri_path }}"""
  clean_source = """{{ clean_source }}"""
  inverse_target = """{{ inverse_target }}"""
  precenter_source = """{{ precenter_source }}"""
  smooth = """{{ smooth }}"""
  reg_type = """{{ reg_type }}"""
  interp = """{{ interp }}"""
  similarity = """{{ similarity }}"""
  optimizer = """{{ optimizer }}"""
  xtol = """{{ xtol }}"""
  ftol = """{{ ftol }}"""
  dry_run = """{{ dry_run }}"""
  
  if os.path.exists(source) and os.path.exists(target):
    
    if clean_source.lower().startswith("f"):
      clean_source = False
    else:
      clean_source = True
    
    if inverse_target.lower().startswith("f"):
      inverse_target = False
    else:
      inverse_target = True
    
    if precenter_source.lower().startswith("f"):
      precenter_source = False
    else:
      precenter_source = True
    
    try:
      smooth = float(smooth)
    except:
      smooth = 0.
    
    if reg_type.lower().startswith("a"):
      reg_type = "affine"
    else:
      reg_type = "rigid"
    
    if interp.lower().startswith("t"):
      interp = "tri"
    else:
      interp = "pv"
    
    similarity = similarity.lower()
    if not similarity in {'cc', 'cr', 'crl1', 'mi', 'nmi', 'slr'}:
      similarity = "crl1"
    
    optimizer = optimizer.lower()
    if not optimizer in { 'powell', 'steepest', 'cg', 'bfgs', 'simplex' }:
      optimizer = "powell"
    
    try:
      xtol = float(xtol)
    except:
      xtol = 0.0001
    
    try:
      ftol = float(ftol)
    except:
      ftol = 0.0001
    
    if dry_run.lower().startswith("t"):
      print(f"""coregistrate_image(
      source = "{source}", 
      target = "{target}", 
      clean_source = {clean_source}, 
      inverse_target = {inverse_target}, 
      precenter_source = {precenter_source},
      smooth={smooth}, 
      reg_type="{reg_type}", 
      interp="{interp}", 
      similarity = "{similarity}", 
      optimizer="{optimizer}",
      xtol={xtol}, 
      ftol={ftol})""")
    else:
      coregistrate_image(
        source = source, target = target, clean_source = clean_source, 
        inverse_target = inverse_target, precenter_source = precenter_source,
        smooth=smooth, reg_type=reg_type, interp=interp, 
        similarity = similarity, optimizer=optimizer, xtol=xtol, ftol=ftol
      )
