import pandas as pd # pandas v. 0.23 required
import numpy as np
import matplotlib.pyplot as plt
import time
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori
from mlxtend.frequent_patterns import association_rules
from sklearn.cluster import KMeans
from sklearn.preprocessing import StandardScaler

#### STEP 1: Load/Clean Data ####
# decompress the .zip file from kaggle
# url is https://www.kaggle.com/jihyeseo/online-retail-data-set-from-uci-ml-repo/home
d_path = '/mnt/ubudata/projects/data/Online Retail.xlsx'
sales = pd.read_excel(d_path)

# determine if the record is an integer and of a particular length
def my_filter(num, length):
    if isinstance(num, int):
        if (len(str(num)))==length:
            return True
    else:
        return False

# only want Invoices that do not start with a C
sales_mod = sales[[str(x)[0] not in ['C'] for x in sales.InvoiceNo]]

def drop_pun(str_var):
    ignore="!\"#$%&\\'()*+,-./:;<=>?@[\\\\]^_`{|}~"
    str_cln=str_var.translate({ord(x): '' for x in ignore})
    return str_cln

# clean up some erroneous characters, typos in the descriptions
sales_mod['Description'] = [drop_pun(str(x)) for x in sales_mod.Description]

#### STEP 2: Transform Data ####
# create a transactional data structure
trans = sales_mod.groupby(['InvoiceNo'])['Description'].agg(list)
trans = [x for x in trans if len(x) > 0]
# convert list of dataframes to transactional
te = TransactionEncoder()
te_trans = te.fit(trans).transform(trans)
# convert back to pandas dataframe
df = pd.DataFrame(te_trans, columns=te.columns_)

#### STEP 3: Apply Association Rules ####

# fairly large dataset, so we will want low support
frequent_itemsets = apriori(
    df,
    min_support=0.01,
    use_colnames=True)

association_rules(
    frequent_itemsets, 
    metric="confidence", 
    min_threshold=0.7).sort_values('lift',ascending=False)

#### STEP 4: Prepare for K-Means ####

# engineer a data structure with features related to each customer
sales_mod['total'] = sales_mod.UnitPrice * sales_mod.Quantity

# total spend by customer
total_rev = sales_mod.groupby(['CustomerID'])['total'].agg(sum).reset_index()

# create a month column
sales_mod['month'] = sales_mod.InvoiceDate.apply(lambda x: str(x)[5:7])

# create a dataframe with total spend by month per customer
cust_month = sales_mod.pivot_table(
    index='CustomerID', 
    columns='month',
    values='total',
    aggfunc='sum',
    fill_value=0)

# create dataframe with counts of each product purchased by customer
cust_products = sales_mod.pivot_table(
    index='CustomerID', 
    columns='StockCode',
    values='Quantity',
    aggfunc='sum',
    fill_value=0)

# join the tables - with customers on rows and features on columns
merge1 = pd.merge(total_rev, cust_month, on='CustomerID')
merge2 = pd.merge(merge1, cust_products, on='CustomerID')
merge2.head()

# scale the variables
scaler = StandardScaler()
customers = merge2['CustomerID']
features = merge2.iloc[:,1:]
merge_scaled = pd.DataFrame(scaler.fit_transform(features))
merge_scaled.columns = features.columns

# transpose the dataframe for kmeans
# columns become the customerIDs, rows are features
merge_s_t = merge_scaled.transpose()
merge_s_t.columns = customers.values

n_clusters = 100
cluster_error = []

start = time.time()
for clust in range(1,n_clusters):
    km = KMeans(n_clusters=clust, random_state=0).fit(merge_s_t)
    cluster_error.append(km.inertia_)
    print(clust)
runtime = time.time() - start
print('Total Runtime: {} sec.'.format(round(runtime,2)))

plt.plot(cluster_error)
plt.show()

# fit it again with the final number of clusters
clusts = 15
km = KMeans(n_clusters=clusts, random_state=0).fit(merged_t)

results_df = pd.DataFrame(list(zip(merged_t.columns,[x for x in km.labels_])))
results_df.columns = ['']

pd.DataFrame(results_df)