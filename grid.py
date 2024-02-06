try:
    from PIL import Image
except ImportError:
    import Image
import numpy as np

import numpy as np
from pathlib import Path

p=Path('/path/to/images/')

for file in p.iterdir():
    if file.suffix==".png":
        # img=plt.imread(str(file))
        grid_width = 1
        img = Image.open(str(file)).convert('RGB')
        array_im = np.array(img)
        dx, dy = 16,16
        # plt.imshow(img)
        x_list = list(range(0,array_im.shape[0],dx))
        y_list = list(range(0,array_im.shape[1],dy))
        tiles = [[array_im[x:x+dx,y:y+dy] for x in x_list] for y in y_list]


        x_listnew_size = array_im.shape[0] + (len(x_list) - 1) * grid_width
        y_listnew_size = array_im.shape[1] + (len(y_list) - 1) * grid_width
        new_image_arr = np.zeros((x_listnew_size, y_listnew_size, array_im.shape[-1]))
        print(new_image_arr.shape)
        for i in range(len(tiles)):
          for j in range(len(tiles[0])):
            x_start = i*grid_width + x_list[i]
            y_start = j*grid_width + y_list[j]
            new_image_arr[x_start:x_start+dx, y_start:y_start+dy] = tiles[j][i]

        save_results_to='/path/to/images'
        # print(np.max(new_image_arr))
        grid_im = Image.fromarray((new_image_arr).astype(np.uint8))
        grid_im.save(save_results_to+ f'{file.stem}_grid_thin.png', "JPEG")
        grid_im.show()
        #display(grid_im)
        
    else:
        pass
