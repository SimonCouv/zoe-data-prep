import pandas as pd

p_path = sys.argv[1]
a_path = sys.argv[2]
pl_path = sys.argv[3]
al_path = sys.argv[4]
mapfile = sys.argv[5]

a = pd.read_csv(a_path, low_memory=False)
p = pd.read_csv(p_path, low_memory=False)
id_map = pd.read_csv(mapfile)


al = a.merge(id_map, left_on="patient_id", right_on="App_ID", how="inner")
pl = p.merge(id_map, left_on="id", right_on="App_ID", how="inner")

al.to_csv(al_path, index=False)
al.to_csv(al_path, index=False)


