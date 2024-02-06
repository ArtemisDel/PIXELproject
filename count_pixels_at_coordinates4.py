from PIL import Image
from itertools import product
from pathlib import Path
import pandas as pd

def detect_black_pixels_in_range(image, coordinate_range):
    """
    Detect black pixels in a range of coordinates of an image.

    Parameters:
    - image: PIL Image object of our text visualisations
    - coordinate_range: List of (x, y) coordinates for every 16 pixels for x and for every 1 pixel for y

    Returns:
    - List of coordinates where the pixel is black. (we only really care about the x-coordinate,
    but the y is necessary for the search in the vertical space.)
    """
    black_coordinates = []
    for coordinates in coordinate_range:
        # print(f'these are the coordinates:{coordinates}')
        pixel_value = image.getpixel(coordinates)
        if pixel_value == 0:
            black_coordinates.append(coordinates)
        else: 
            pass
    # print(f'black coordinates:{black_coordinates}')
    return black_coordinates

#make list of all coordinates for 16x16, change pixel_num to refect different pixel dimensions
def get_coordinates_list(pixel_num):
    '''
    Creates a list of the necessary coordinates depending on the pixel number.
    It assumes your image is square. 
    
    Parameters:
    - pixel_num: number of pixels on each side of your image
    - x-list: list of x coordinates from 0 to 255 going up every pixel_num step
    - y-list: list of y coordinates from 0 to 255 going up every 1 step to cover entire vertical line

    Returns:
    - List of coordinates where to search for black pixels that indicate a cut-off point. 
    '''
    x_max=(pixel_num*pixel_num)-pixel_num # ! do this to not count edge of image, cutoff shows in 0 point.
    y_max=pixel_num*pixel_num
    x_list=range(0,x_max, pixel_num)
    y_list=range(0,y_max,1)
    coordinate_list=list(product(x_list,y_list))
    # print(f'this is the coordinate_list:{coordinate_list} ')
    return coordinate_list


#get your coordinate list object
coordinate_list=get_coordinates_list(16)

#iterate over png files in your directory and check for black pixels in cutoff coordinates. 
def check_for_cutoffs(path,coordinate_list):
      '''
    Takes the path to a folder directory, opens each image in the directory and searches for black pixels at the specified coordinate_list.
    If black pixels are detected at the specified coordinates they are appended to a list corresponding to the image in a dictionary.
    
    Parameters:
    - path: path to specified directory with images
    - coordinate-list: a list of coordinates (list of tuples)

    Returns:
    - Dictionary where each image name corresponds to a list of coordinates where a black pixel was detected in the image.
    '''
    directory=Path(path)
    cutoff_dict={}
    for file in directory.iterdir():
        if file.suffix==".png":
            image=Image.open(file)
            thresh = 200
            fn = lambda x : 255 if x > thresh else 0
            image= image.convert('L').point(fn, mode='1') #convert to black& white
            # image.show()
            # print(f'an image:{list(image.getdata())}')
            black_coordinates = detect_black_pixels_in_range(image, coordinate_list)
            cutoff_dict[file.name]=black_coordinates
    return cutoff_dict





# Check for black pixels in the specified range of coordinates
# * use this to show what happens when 256 coordinate is used:
# * test=[(48, 256)]
# black_coordinates = detect_black_pixels_in_range(image, coordinate_list)
# print(type(black_coordinates))

#get the dictionary of images and their cutoff points list
cutoff_dict=check_for_cutoffs("path/to/visualisation/",coordinate_list)
# print(f'this is the cutoff_dict:{cutoff_dict}')

#convert dictionary to dataframes with pandas
df = pd.DataFrame.from_dict(cutoff_dict, orient="index")
# print(f"The dataframe:{df}")

# save the dataframe as csv 
df.to_csv('output.csv', index=True)


# if cutoff_dict:
#     print(f"Black pixels found at the following coordinates: {cutoff_dict}")
# else:
#     print("No black pixels found in the specified range.")
