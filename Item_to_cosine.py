import pandas as pd 
import numpy as np
from sentence_transformers import SentenceTransformer, util

class PsychologicalTestAnalyzer:
    
    def __init__(self, models, model_short):

        """
        Initializes the analyzer with embedding models.
        :param models: List of sentence transformer model names.
        :param model_short: Shortened names for output file naming.
        """
        self.models = models
        self.model_short = model_short
        self.loaded_models = {mod: SentenceTransformer(mod) for mod in models}  # Load models once

    def generate_embeddings(self, df_items):
        if df_items.empty:
            print("Warning: DataFrame is empty. Skipping...")
            return None
        """
        Computes sentence embeddings for all items.
        :param df_items: DataFrame containing psychological test items.
        :return: DataFrame with embeddings for each model.
        """
        for mod in self.models:
            item_embed = []
            
            for item in range(len(df_items)):
                encoded_item = self.loaded_models[mod].encode(df_items['Item'].iloc[item])
                item_embed.append(encoded_item)
            
            # Store embeddings in DataFrame
            df_items[mod + '_embeddings'] = item_embed
        
        return df_items

    def compute_cosine_similarity(self, df_items):
        if df_items.empty:
            print("Warning: DataFrame is empty. Skipping...")
            return None
        """
        Computes cosine similarity matrices and saves them.
        :param df_items: DataFrame with computed embeddings.
        :param output_dir: Directory to store cosine similarity matrices.
        """
        matrices = []
        for i, mod in enumerate(self.models):
            cosine_sim_matrix = util.pytorch_cos_sim(df_items[mod + '_embeddings'], df_items[mod + '_embeddings']).numpy()
            
            # Fill diagonal with 1 to avoid EFA functions treating similarity as covariance
            np.fill_diagonal(cosine_sim_matrix, 1)

            # Save the matrix as CSV
            matrices.append(pd.DataFrame(
                cosine_sim_matrix, 
                columns=df_items['Item'].unique(), 
                index=df_items['Item'].unique()
            ))
        return matrices

    def analyze_tests(self, test_dataframes):
        """
        Loops through all test dataframes, computes embeddings, and cosine similarities.
        :param test_dataframes: Dictionary of test names and corresponding DataFrames.
        :return: Dictionary of test names and their cosine similarity matrices.
        """
        results = {}  # Store results
        for test_name, df in test_dataframes.items():
            print(f"Processing: {test_name}")
            df = self.generate_embeddings(df)  # Generate embeddings
            results[test_name] = self.compute_cosine_similarity(df)  # Store matrices

        return results  # Return all processed tests
