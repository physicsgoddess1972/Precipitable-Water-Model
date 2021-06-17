#!/bin/bash

while getopts "mdb" opt; do
	case "${opt}" in
    m)
      echo "Deploying ML Dashboard"
      cd ./pw_ml_dash/
      /opt/google-cloud*/bin/gcloud config set project pw-ml-dash
      /opt/google-cloud*/bin/gcloud app deploy
      echo "Complete";;
    d)
      echo "Deploying Data Dashboard"
      cd ./pw_data_dash/
      gcloud config set project pw-data-dash
      gcloud app deploy
      echo "Complete";;

    m)
      echo "Deploying Map Dashboard"
      cd ./pw_map_dash/
      gcloud config set project pw-map-dash
      gcloud app deploy
      echo "Complete";;
    b)
      echo "Deploying ML Dashboard"
      cd ./pw_ml_dash/
      gcloud config set project pw-ml-dash
      gcloud app deploy
      echo "Deploying Data Dashboard"
      cd ../pw_data_dash/
      gcloud config set project pw-data-dash
      gcloud app deploy
      echo "Deploying Map Dashboard"
      cd ./pw_map_dash/
      gcloud config set project pw-map-dash
      gcloud app deploy
      echo "Complete";;
  esac
done
