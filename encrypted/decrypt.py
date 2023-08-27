import sys

for filename in sys.argv[1:]:
	cl_file = open(f'{filename}.CL', 'rb')
	out_file = open(f'out/{filename}.cl', 'wb')

	out_data = []
	for c in cl_file.read():
		c = (c ^ 0xFF) + 0x20
		
		# Needs & 0xFF because some bytes end up as 0x0116. What’s up with that?
		out_data.append(c & 0xFF)

	out_file.write(bytes(out_data))

	cl_file.close()
	out_file.close()
	print(f'Done with {filename}!')

print('\nDone with every file!!!!')
