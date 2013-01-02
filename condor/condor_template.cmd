universe        = vanilla
executable      = ${cur_dir}/../Cycling
output          = ${cur_dir}/raw_output/${filepath}.$(Process).out
error           = ${cur_dir}/raw_output/${filepath}.$(Process).err
log             = ${cur_dir}/raw_output/${filepath}.$(Process).log
arguments 		= ${cur_dir}/${filepath}

Requirements	= DoC_OS_Distribution == "Ubuntu" && \
                  DoC_OS_Release == "12.04"

queue ${times}
