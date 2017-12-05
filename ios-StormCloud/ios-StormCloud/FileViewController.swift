//
//  FileViewController.swift
//  ios-StormCloud
//
//  Created by Jack Thias on 12/4/17.
//  Copyright © 2017 Jack Thias. All rights reserved.
//

import UIKit

class FileViewController : UIViewController, UINavigationControllerDelegate, UIImagePickerControllerDelegate {
    @IBOutlet weak var scrollView : UIScrollView!
    
    @IBAction func importImage(_ sender: AnyObject) {
        let image = UIImagePickerController()
        image.delegate = self
        
        image.sourceType = UIImagePickerControllerSourceType.photoLibrary
        image.allowsEditing = false
        
        self.present(image, animated: true) {
            //blah
        }
    }
    
    var xPosition : CGFloat = 0
    var yPosition : CGFloat = 0
    var imageWidth : CGFloat = 0
    var imageHeight : CGFloat = 0
    var scrollViewSize : CGFloat = 0
    
    override func viewDidLoad() {
        imageWidth = self.view.frame.width / 2.0
        print("Frame Width:\t\(scrollView.frame.width)\nImage Width:\t\(imageWidth)\nSafe Area:\t\t\(self.view.frame.width)")
        imageHeight = imageWidth
        scrollView.showsHorizontalScrollIndicator = false
        scrollView.showsVerticalScrollIndicator = true
        scrollView.alwaysBounceVertical = true
        scrollView.isScrollEnabled = true
    }
    
    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : Any]) {
        let image = info[UIImagePickerControllerOriginalImage] as! UIImage
        renderImage(image)
        imageUploadRequest(imageView: image, uploadUrl: NSURL(string: "http://10.11.195.133:4000/store")!, param: [:])
        self.dismiss(animated: true, completion: nil)
    }
    
    func imageUploadRequest(imageView: UIImage, uploadUrl: NSURL, param: [String:String]?) {
        
        let request = NSMutableURLRequest(url:uploadUrl as URL);
        request.httpMethod = "POST"
        
        let boundary = generateBoundaryString()
        
        request.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")
        
        let imageData = UIImageJPEGRepresentation(imageView, 1)
        
        if(imageData==nil)  { return; }
        
        request.httpBody = createBodyWithParameters(parameters: param, filePathKey: "file", imageDataKey: imageData! as NSData, boundary: boundary) as Data
        
        //myActivityIndicator.startAnimating();
        
        let task =  URLSession.shared.dataTask(with: request as URLRequest,
                                                                     completionHandler: {
                                                                        (data, response, error) -> Void in
                                                                        if let data = data {
                                                                            
                                                                            // You can print out response object
                                                                            print("******* response = \(String(describing: response))")
                                                                            
                                                                            print(data.count)
                                                                            // you can use data here
                                                                            
                                                                            // Print out reponse body
//                                                                            let responseString = NSString(data: data, encoding: String.Encoding.utf8.rawValue)
//                                                                            print("****** response data = \(responseString!)")
//
//                                                                            let json =  try!JSONSerialization.jsonObject(with: data, options: .mutableContainers) as? NSDictionary
//
//                                                                            print("json value \(String(describing: json))")
                                                                            
                                                                            //var json = NSJSONSerialization.JSONObjectWithData(data, options: .MutableContainers, error: &err)
                                                                            
//                                                                            let dispatch = DispatchQueue()
//                                                                            let group : DispatchGroup
//                                                                            group.setTarget(queue: dispatch)
//                                                                            dispatch.async(group: group, execute: {
//                                                                                //self.myActivityIndicator.stopAnimating()
//                                                                                //self.imageView.image = nil;
//                                                                            });
                                                                            
                                                                        } else if let error = error {
                                                                            print(error.localizedDescription)
                                                                        }
        })
        task.resume()
        
        
    }
    
    func renderImage(_ image: UIImage) {
        let myImageView = UIImageView(image: image)
        myImageView.frame.size.width = imageWidth
        myImageView.frame.size.height = imageHeight
        myImageView.frame.origin.x = xPosition
        myImageView.frame.origin.y = yPosition
        
        scrollView.addSubview(myImageView)
        xPosition += imageWidth
        
        if xPosition + imageWidth > view.frame.width {
            print("\(xPosition + imageWidth) > \(view.frame.width)")
            xPosition = 0
            yPosition += imageHeight
        }
        scrollViewSize += imageWidth
    }
    
    func createBodyWithParameters(parameters: [String: String]?, filePathKey: String?, imageDataKey: NSData, boundary: String) -> NSData {
        let body = NSMutableData();
        
        if parameters != nil {
            for (key, value) in parameters! {
                body.appendString(string: "--\(boundary)\r\n")
                body.appendString(string: "Content-Disposition: form-data; name=\"\(key)\"\r\n\r\n")
                body.appendString(string: "\(value)\r\n")
            }
        }
        
        let filename = "user-profile.jpg"
        
        let mimetype = "image/jpg"
        
        body.appendString(string: "--\(boundary)\r\n")
        body.appendString(string: "Content-Disposition: form-data; name=\"\(filePathKey!)\"; filename=\"\(filename)\"\r\n")
        body.appendString(string: "Content-Type: \(mimetype)\r\n\r\n")
        body.append(imageDataKey as Data)
        body.appendString(string: "\r\n")
        
        body.appendString(string: "--\(boundary)--\r\n")
        
        return body
    }
    
    func generateBoundaryString() -> String {
        return "Boundary-\(NSUUID().uuidString)"
    }
    
}// extension for impage uploading

extension NSMutableData {
    
    func appendString(string: String) {
        let data = string.data(using: String.Encoding.utf8, allowLossyConversion: true)
        append(data!)
    }
}

