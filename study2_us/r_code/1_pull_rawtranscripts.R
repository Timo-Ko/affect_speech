## pull raw transcript data

# This is the gs util path of the bucket
bucket_path = 'gs://sable_data_2018/FA18_All_participants.Audio_Text_only'

# create a list of all rds files in the bucket

list_cmd = paste("gsutil ls -r ", bucket_path, "**.RDS", sep = "")
list_of_files = system(list_cmd, intern = TRUE)

# number of text files 
length(list_of_files) # 880 participants

# define file name
base_name = "121f7uao" # define participant
file_name = paste(base_name, ".RDS", sep = '')

file_name 

# create the respective download command 

download_command = paste("gsutil cp gs://sable_data_2018/FA18_All_participants.Audio_Text_only/", file_name, " ./audio_transcripts", sep = "") # download selected file

download_command_all = paste("gsutil cp -r gs://sable_data_2018/FA18_All_participants.Audio_Text_only", " ./audio_transcripts", sep = "") # download ALL files

# download entire folder
# gsutil cp -r gs://bucket/folder .


download_command_all

# create new directory to store data downloaded files 
# this is only needed once
#dir.create("./audio_transcripts")

# download file(s)
system(download_command_all)