from sklearn.cluster import KMeans
import numpy as np
import matplotlib.pyplot as plt

# Read the data
with open('./Jabenjy_Sudoku/steps.txt', 'r') as file:
    data = [int(line.strip()) for line in file]

filtered_data = [x for x in data if x <= 6000]

# Convert data to numpy array
data = np.array(filtered_data).reshape(-1, 1)

# Instantiate KMeans object
kmeans = KMeans(n_clusters=4)

# Fit the data to the model
kmeans.fit(data)

# Get the cluster centers
cluster_centers = kmeans.cluster_centers_

# Get the cluster labels
labels = kmeans.labels_

# Analyze the results
for i, center in enumerate(cluster_centers):
    print(f"Cluster {i+1} center: {int(center)}")
    
plt.figure(figsize=(10, 6))
plt.scatter(range(len(data)), data, c=labels, cmap='viridis', alpha=0.5)
plt.scatter(range(len(cluster_centers)), cluster_centers, c='red', marker='x', label='Cluster Centers')

# # Read the data for kaggle_steps
# with open('./kaggle_steps.txt', 'r') as file:
#     kaggle_data = [int(line.strip()) for line in file]
#     kaggle_data = [x for x in kaggle_data if x <= 6000]

# # Plot kaggle steps points
# plt.scatter(range(len(data), len(data) + len(kaggle_data)), kaggle_data, c='blue', alpha=0.5, label='Kaggle Steps')

# Read the data for kaggle_steps
with open('./17_steps.txt', 'r') as file:
    kaggle_data = [int(line.strip()) for line in file]
    kaggle_data = [x for x in kaggle_data if x <= 6000]

# Plot kaggle steps points
plt.scatter(range(len(data), len(data) + len(kaggle_data)), kaggle_data, c='blue', alpha=0.5, label='17 Steps')

plt.xlabel('Data Index')
plt.ylabel('Counts')
plt.title('K-Means Clustering')
plt.legend()
plt.savefig('kmeans+17.png')
plt.show()
