import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sb
import numpy as np
import sys
# Load data from CSV
df = pd.read_csv(sys.argv[1], names=['STATE', 'ACTION', 'VALUE'],
                 index_col=0, sep=' ')

df_m = df.copy()
df_m = df_m.groupby(['STATE', 'ACTION']).sum()
df_m = df_m.unstack(level=0)
fig, ax = plt.subplots(figsize=(40, 9))
sb.heatmap(df_m, cmap="Blues",  cbar_kws={"shrink": .8})
plt.show()