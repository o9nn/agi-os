# Deployment Guide - Deep Tree Echo AGI Avatar

**Version:** 1.0.0  
**Date:** December 2, 2025

This guide provides step-by-step instructions for deploying the Deep Tree Echo AGI avatar features from the `echo9llama` repository to the `UnrealEngineCog` repository.

## 1. Prerequisites

-   A local clone of the `UnrealEngineCog` repository.
-   All assets from the `echo9llama` repository (provided in the `echo9llama_improvements.zip` archive).

## 2. Asset Integration

### 2.1. Live2D Models

1.  Create a `Live2DModels` directory inside the `UnrealEngineCog/Content` directory.
2.  Copy the contents of the `echo9llama/Content/Live2DModels` directory to `UnrealEngineCog/Content/Live2DModels`.

### 2.2. Cubism SDK

1.  Create a `Plugins` directory inside the `UnrealEngineCog` directory if it doesn't already exist.
2.  Copy the `CubismSdkForUnrealEngine-5-r.1-beta.1` directory from `echo9llama/Plugins` to `UnrealEngineCog/Plugins`.

### 2.3. Profile Picture Textures

1.  Create a `UI/Textures` directory inside the `UnrealEngineCog/Content` directory.
2.  Copy the contents of the `echo9llama/Content/UI/Textures` directory to `UnrealEngineCog/Content/UI/Textures`.

## 3. Source Code Integration

1.  Copy the following directories from `echo9llama/Source` to `UnrealEngineCog/Source`:
    -   `AssetManagement`
    -   `Avatar`
    -   `Environment`
    -   `Live2DCubism`
    -   `Personality`

## 4. Project Configuration

1.  Open the `UnrealEngineCog.uproject` file in a text editor.
2.  Add the `Live2DCubism` plugin to the `Plugins` section:

    ```json
    "Plugins": [
        {
            "Name": "Live2DCubism",
            "Enabled": true
        }
    ]
    ```

3.  Add the new source code modules to the `Modules` section:

    ```json
    "Modules": [
        // ... existing modules
        {
            "Name": "AssetManagement",
            "Type": "Runtime",
            "LoadingPhase": "Default"
        },
        {
            "Name": "Avatar",
            "Type": "Runtime",
            "LoadingPhase": "Default"
        },
        {
            "Name": "Environment",
            "Type": "Runtime",
            "LoadingPhase": "Default"
        },
        {
            "Name": "Live2DCubism",
            "Type": "Runtime",
            "LoadingPhase": "Default"
        },
        {
            "Name": "Personality",
            "Type": "Runtime",
            "LoadingPhase": "Default"
        }
    ]
    ```

## 5. GitHub Actions Workflows

1.  Create a `.github/workflows` directory in the `UnrealEngineCog` repository.
2.  Copy the following files from `echo9llama/.github/workflows` to `UnrealEngineCog/.github/workflows`:
    -   `main-ci.yml`
    -   `live2d-integration-test.yml`
    -   `avatar-system-test.yml`
    -   `release.yml`

## 6. Build and Test

1.  Generate Visual Studio project files for `UnrealEngineCog.uproject`.
2.  Build the project in Visual Studio.
3.  Run the editor and verify that the new assets and components are available.
4.  Push the changes to a new branch and create a pull request to trigger the GitHub Actions workflows.


