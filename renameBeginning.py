# importing os module 
import os 
  
# Function to rename the beginning of multiple files
def main(): 
  
    data_folder = "C:/Users/finli/Desktop/gun_Sarah"
    replacement_count = 0
    for count, filename in enumerate(os.listdir(data_folder)): 
       
        print(filename)
        google = filename.rfind("[www.google.com]")
        
        if (google != -1): # -1 means the string wasn't found
            replacement_count = replacement_count + 1
            base = filename[google+16:]
            print(base)
            os.rename(data_folder +"/"+ filename, data_folder +"/"+ base ) 
        
    print("Changed" , replacement_count , "file names")
# Driver Code 
if __name__ == '__main__': 
      
    # Calling main() function 
    main() 