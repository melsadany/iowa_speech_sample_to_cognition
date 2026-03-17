### run this on EDL conda environment, and make your working directory the pwesuite folder in workbench
### i.e. on python, not python3
# EDL
# cd /wdata/msmuhammad/workbench/pwesuite


import pandas as pd
from models.metric_learning.preprocessor import preprocess_dataset_foreign
from models.metric_learning.model import RNNMetricLearner
import torch
import tqdm
import math

# === LOAD CSV ===
df = pd.read_csv("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/data/derivatives/PWE/words.csv")
if df.columns[0] != "word":
    df.columns = ["word"]

words = df["word"].tolist()

# === PREPROCESS ===
data = preprocess_dataset_foreign(
    [{"token_ort": word, "token_ipa": None} for word in words],
    features="token_ort"
)

# === LOAD MODEL ===
model = RNNMetricLearner(dimension=300, feature_size=data[0][0].shape[1])
model.load_state_dict(torch.load("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/pwesuite/computed/models/rnn_metric_learning_token_ort_all.pt"))  # adjust path if needed
model.eval()

# === GET EMBEDDINGS ===
BATCH_SIZE = 32
embeddings = []

for i in tqdm.tqdm(range(math.ceil(len(data) / BATCH_SIZE))):
    batch = [f for f, _ in data[i * BATCH_SIZE:(i + 1) * BATCH_SIZE]]
    batch_out = model.forward(batch).detach().cpu().numpy()
    embeddings.extend(batch_out)

# === SAVE TO CSV ===
embedding_df = pd.DataFrame(embeddings)
embedding_df["word"] = words
embedding_df.to_csv("/Dedicated/jmichaelson-wdata/msmuhammad/git-repo/iowa_speech_sample_analysis/data/derivatives/PWE/raw-embeddings.csv", index=False)
