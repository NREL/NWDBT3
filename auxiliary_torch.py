import pandas as pd
import glob
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, TensorDataset
import numpy as np
from sklearn import preprocessing
from sklearn.model_selection import train_test_split

# Define the pinball loss function
def pinball_loss(preds, targets, quantiles):
    losses = []
    for i, q in enumerate(quantiles):
        errors = targets - preds[:, i] # errors = targets[i] - preds[:, i] change it to enumerate(quantiles)
        # errors = targets[i] - preds[i, :] # change it to enumerate(preds)
        loss_q = torch.mean(torch.max((q - 1) * errors, q * errors))
        losses.append(loss_q)
    loss = torch.mean(torch.stack(losses))
    return loss

# Define the pinball loss function
def pinball_loss1(preds, targets, quantiles, batch_size):
    losses = []
    for i, q in enumerate(quantiles):
        errors = targets - preds[:, i].view(-1,1) # errors = targets[i] - preds[:, i] change it to enumerate(quantiles)
        # errors = targets[i] - preds[i, :] # change it to enumerate(preds)
        loss_q = torch.mean(torch.max((q - 1) * errors, q * errors))
        losses.append(loss_q)
    loss = torch.mean(torch.stack(losses))
    return loss

def combined_loss(preds, targets, quantiles, batch_size, alpha=0.5):
    """
    Calculate a combined loss of Pinball Loss and RMSE.
    
    Parameters:
    - preds: Predicted values.
    - targets: Actual values.
    - quantiles: List of quantiles.
    - batch_size: Number of samples in a batch.
    - alpha: Weight for the RMSE in the combined loss. (0 <= alpha <= 1)
    
    Returns:
    - Combined loss
    """
    
    # Calculate Pinball Loss
    losses = []
    for i, q in enumerate(quantiles):
        errors = targets - preds[:, i].view(-1,1)
        loss_q = torch.mean(torch.max((q - 1) * errors, q * errors))
        losses.append(loss_q)
    pinball = torch.mean(torch.stack(losses))
    
    # Calculate RMSE
    mean_preds = torch.mean(preds, dim=1)
    mse = torch.mean((mean_preds - targets) ** 2)
    rmse = torch.sqrt(mse)
    
    # Combine the losses
    combined = alpha * rmse + (1 - alpha) * pinball
    
    return combined

# Define the training function
def train_model_batch(model, optimizer, dataloader, quantiles, num_epochs):
    model.train()
    for epoch in range(num_epochs):
        train_loss = 0
        for batch_idx, (data, target) in enumerate(dataloader):
            optimizer.zero_grad()
            # Forward pass
            output = model(data)
            # Compute the loss
            loss = pinball_loss(output, target, quantiles)
            train_loss += loss.item()
            # Backward pass
            loss.backward()
            optimizer.step()
        avg_loss = train_loss / len(dataloader)
        # Print progress
    if (epoch+1) % 10 == 0:
        print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {avg_loss:.4f}')

# Define the training function
def train_model_batch_saveloss(model, optimizer, dataloader_train, dataloader_test, quantiles, num_epochs):
    loss_train=  []
    loss_validate = []
    for epoch in range(num_epochs):
        train_loss = 0
        test_loss = 0
        model.train()
        for batch_idx, (data, target) in enumerate(dataloader_train):
            optimizer.zero_grad()
            # Forward pass
            output = model(data)
            # Compute the loss
            loss = pinball_loss1(output, target, quantiles, 64)
            # loss = combined_loss(output, target, quantiles, 64)
            # loss = pinball_loss1(output, target, quantiles)
            train_loss += loss.item()
            # Backward pass
            loss.backward()
            # print(f'The loss of this batch is {loss.item()}')
            optimizer.step()
        avg_loss = train_loss / len(dataloader_train)
        loss_train.append(avg_loss)
        # Print progress
        if (epoch+1) % 10 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {avg_loss:.4f}')
        model.eval()     # Optional when not using Model Specific layer
        for batch_idx, (data, target) in enumerate(dataloader_test):
            # Forward pass
            output = model(data)
            # Compute the loss
            # loss = pinball_loss1(output, target, quantiles)
            loss = pinball_loss1(output, target, quantiles, 64)
            # loss = combined_loss(output, target, quantiles, 64)
            test_loss +=loss.item()
        avg_loss = test_loss / len(dataloader_test)
        loss_validate.append(avg_loss)
        if (epoch+1) % 10 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss_val: {avg_loss:.4f}')
    return loss_train, loss_validate

def train_model_batch_saveloss_combine(model, optimizer, dataloader_train, dataloader_test, quantiles, num_epochs, alpha):
    loss_train=  []
    loss_validate = []
    for epoch in range(num_epochs):
        train_loss = 0
        test_loss = 0
        model.train()
        for batch_idx, (data, target) in enumerate(dataloader_train):
            optimizer.zero_grad()
            # Forward pass
            output = model(data)
            # Compute the loss
            # loss = pinball_loss1(output, target, quantiles, 64)
            loss = combined_loss(output, target, quantiles, 64, alpha)
            # loss = pinball_loss1(output, target, quantiles)
            train_loss += loss.item()
            # Backward pass
            loss.backward()
            # print(f'The loss of this batch is {loss.item()}')
            optimizer.step()
        avg_loss = train_loss / len(dataloader_train)
        loss_train.append(avg_loss)
        # Print progress
        if (epoch+1) % 10 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {avg_loss:.4f}, Alpha: {alpha}')
        model.eval()     # Optional when not using Model Specific layer
        for batch_idx, (data, target) in enumerate(dataloader_test):
            # Forward pass
            output = model(data)
            # Compute the loss
            # loss = pinball_loss1(output, target, quantiles)
            # loss = pinball_loss1(output, target, quantiles, 64)
            loss = combined_loss(output, target, quantiles, 64, alpha)
            test_loss +=loss.item()
        avg_loss = test_loss / len(dataloader_test)
        loss_validate.append(avg_loss)
        if (epoch+1) % 10 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss_val: {avg_loss:.4f}')
    return loss_train, loss_validate

def data_aug_dup_high_ws(df,ws,seed=3):
    df1 = []
    times = 0
    while times<seed:
        df1.append(df[df.ws>ws].copy())
        times+=1
    df1 = pd.concat(df1, axis=0, ignore_index=True)
    df2 = pd.concat((df,df1), axis=0, ignore_index=True)
    return df2

def data_aug_dup_low_ws(df,ws,seed=3):
    df1 = []
    times = 0
    while times<seed:
        df1.append(df[df.ws<ws].copy())
        times+=1
    df1 = pd.concat(df1, axis=0, ignore_index=True)
    df2 = pd.concat((df,df1), axis=0, ignore_index=True)
    return df2


# Define the training function
def train_model_epoch(model, optimizer, X_train, y_train, quantiles, num_epochs):
    model.train()
    for epoch in range(num_epochs):
        # Forward pass
        preds = model(X_train)
        # Compute the loss
        loss = pinball_loss(preds, y_train, quantiles)
        # Backward pass
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()
        # Print progress
        if (epoch+1) % 10 == 0:
            print(f'Epoch [{epoch+1}/{num_epochs}], Loss: {loss.item():.4f}')
            
def listfiles(path):
    """
    Parameters
    path : folder in the current directory contains csv files.
    -------
    Returns
    files : file path for future pd.csv_read
    houses : House_id
    """
    files = []
    turbines = []
    files = glob.glob(path+"*.csv")
    if not files:
        files = glob.glob(path+"*.xlsx")
        for i in range(len(files)):
            pos = -6
            while files[i][pos].isdigit():
                pos-=1
            turbines.append(files[i][pos:-5])
        return files, turbines
    for i in range(len(files)):
        pos = -5
        while files[i][pos].isdigit():
            pos-=1
        files[i] = files[i][pos:-4]
    return files