# this can separate one file into testing and training folders

# importing os module 
import os 
import shutil

def main(): 
    folder_path = "C:/Users/finli/Desktop/test/"
    train_percent = 80 # must be multiple of 10
    val_percent = 20 # manually change code if these numbers change

    for count, filename in enumerate(os.listdir(folder_path)):
        print("count", count)
        extension = filename.split('.', 1)[1]
        if (extension == "txt"):
            print("extension", extension)
        
            name = filename.split('.')[0]
            print(name)

            if(count%10 == 0):
                new_path = "C:/Users/finli/Desktop/val_data/" 
            elif(count%10 == 1):
                new_path = "C:/Users/finli/Desktop/val_data/" 
            else:
                new_path = "C:/Users/finli/Desktop/train_data/"  
        
            #print(new_path)

            old_image_path = os.path.join(folder_path, name + '.jpg')
            new_image_path = os.path.join(new_path, name + ".jpg")
        
            old_text_path = os.path.join(folder_path, name + ".txt")
            new_text_path = os.path.join(new_path, name + ".txt")
        
            print(old_text_path, new_text_path)
            shutil.copy2(old_image_path, new_image_path) 
            shutil.copy2(old_text_path, new_text_path)

if __name__ == '__main__': 
      
    # Calling main() function 
    main() 