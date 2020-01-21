import os
import subprocess

decades = [i for i in range(4, 10)] + [0]

for decade in decades:
    decade_prefix = '19' + decade.__str__()
    d = decade_prefix + '0'
    years = [decade_prefix + str(i) for i in range(10)]

    for year in years:
        cmd = ['bevent.exe', '-y', year]
        eve_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), d + 'seve', year)
        files = [f for f in os.listdir(eve_dir) if (os.path.isfile(os.path.join(eve_dir, f)) and '.EV' in f)]

        for f_name in files:
            print(f_name)
            with open(os.path.join(eve_dir, 'csv', f_name + '.csv'), 'w') as out_file:
                full_cmd = cmd + [f_name]
                print(full_cmd, eve_dir)
                subprocess.call(full_cmd, stdout=out_file, cwd=eve_dir)


