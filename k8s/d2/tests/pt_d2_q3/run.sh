kubectl apply -f pt_d2_q3.yaml
kubectl get deployments
kubectl set image deployment kplabs-updates webserver=nginx:alpine --record
kubectl get deployments
kubectl rollout undo deployments kplabs-updates --to-revision 1
kubectl get deployments
kubectl rollout history deployments kplabs-updates

