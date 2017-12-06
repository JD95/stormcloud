//
//  FileViewController.swift
//  ios-StormCloud
//
//  Created by Jack Thias on 12/4/17.
//  Copyright © 2017 Jack Thias. All rights reserved.
//

import UIKit

extension Formatter {
    static let iso8601: DateFormatter = {
        let formatter = DateFormatter()
        formatter.calendar = Calendar(identifier: .iso8601)
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(secondsFromGMT: 0)
        formatter.dateFormat = "yyyyMMddHHmmssSSSXXXXX"
        return formatter
    }()
}
extension Date {
    var iso8601: String {
        return Formatter.iso8601.string(from: self)
    }
}

extension String {
    var dateFromISO8601: Date? {
        return Formatter.iso8601.date(from: self)   // "Mar 22, 2017, 10:22 AM"
    }
}

class FileViewController : UIViewController, UINavigationControllerDelegate, UIImagePickerControllerDelegate {
    @IBOutlet weak var scrollView : UIScrollView!
    @IBOutlet weak var fakeImageReadIn : UIImageView!
    
    @IBAction func importImage(_ sender: AnyObject) {
        let image = UIImagePickerController()
        image.delegate = self
        
        image.sourceType = UIImagePickerControllerSourceType.photoLibrary
        image.allowsEditing = false
        
        self.present(image, animated: true) {
            //blah
        }
    }
    
    @objc func unBlur(_ sender: AnyObject) {
        sender.superview??.subviews[0].subviews[0].isHidden = true
        (sender.self as! UIButton).isHidden = true
    }
    
    @objc func deleteImage(_ sender: AnyObject) {
        //delete the parent view and reorientate everything
        print("Deleting image...")
        _ = deleteImageRequest(URL(string: "http://10.11.195.133:4000/delete/\(sender.superview??.accessibilityLabel! ?? "blah")")!, param: [:])
        sender.superview??.isHidden = true
    }
    
    var xPosition : CGFloat = 0
    var yPosition : CGFloat = 0
    var imageWidth : CGFloat = 0
    var imageHeight : CGFloat = 0
    var scrollViewSize : CGFloat = 0
    
    override func viewDidLoad() {
        imageWidth = self.view.frame.width / 2.0
        imageHeight = imageWidth
        scrollView.showsHorizontalScrollIndicator = false
        scrollView.showsVerticalScrollIndicator = true
        scrollView.alwaysBounceVertical = true
        scrollView.isScrollEnabled = true
        _ = downloadImage(URL(string: "http://10.11.195.133:4000/retrieve/tiger/)")!, param: [:])
        renderImage(fakeImageReadIn.image!, name: "tiger")
    }
    
    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [String : Any]) {
        let image = info[UIImagePickerControllerOriginalImage] as! UIImage
        
//        let calendar = Calendar.current
        let datetime = Date().iso8601
        let imgName = datetime+MyAppDelegate.idToken
        renderImage(image, name: imgName)
        print("Look at this shit: http://10.11.195.133:4000/store/\(datetime+MyAppDelegate.idToken)/")
        imageUploadRequest(imageView: image, uploadUrl: NSURL(string: "http://10.11.195.133:4000/store/\(imgName)/")!, param: [:])
        
        self.dismiss(animated: true, completion: nil)
    }
    
    func downloadImage(_ downloadUrl: URL, param: [String:String]?) -> Bool {
        let session = URLSession.shared
        var request = URLRequest(url: downloadUrl)
        request.httpMethod = "GET"
        
        print(request)
        let task = session.dataTask(with: request as URLRequest, completionHandler: { data, response, error in
            guard error == nil else {
                return
            }
            
            guard let data = data else {
                return
            }
            
            do {
                if let json = try JSONSerialization.jsonObject(with: data, options: .mutableContainers) as? [String: Any] {
                    print(json)
                }
            } catch let error {
                print(error.localizedDescription)
                return
            }
        })
        task.resume()
        print(task.response ?? (task.error ?? "No error or responce."))
        return true
    }
    
    func deleteImageRequest(_ downloadUrl: URL, param: [String:String]?) -> Bool {
        let session = URLSession.shared
        var request = URLRequest(url: downloadUrl)
        request.httpMethod = "GET"
        
        print(request)
        let task = session.dataTask(with: request as URLRequest, completionHandler: { data, response, error in
            guard error == nil else {
                return
            }
            
            guard let data = data else {
                return
            }
            
            do {
                if let json = try JSONSerialization.jsonObject(with: data, options: .mutableContainers) as? [String: Any] {
                    print(json)
                }
            } catch let error {
                print(error.localizedDescription)
                return
            }
        })
        task.resume()
        print(task.response ?? (task.error ?? "No error or responce."))
        return true
    }
    
    func imageUploadRequest(imageView: UIImage, uploadUrl: NSURL, param: [String:String]?) {
        
        let request = NSMutableURLRequest(url:uploadUrl as URL);
        request.httpMethod = "POST"
        
        let boundary = generateBoundaryString()
        
        request.setValue("multipart/form-data; boundary=\(boundary)", forHTTPHeaderField: "Content-Type")
        
        let imageData = UIImageJPEGRepresentation(imageView, 1)
        
        if(imageData==nil)  { return; }
        
        request.httpBody = createBodyWithParameters(parameters: param, filePathKey: "file", imageDataKey: imageData! as NSData, boundary: boundary) as Data
        
        let task = URLSession.shared.dataTask(with: request as URLRequest,
                                              completionHandler: {
                                                    (data, response, error) -> Void in
                                                    if let data = data {
                                                        // You can print out response object
                                                        print("******* response = \(String(describing: response))")
                                                        
                                                        print(data.count)
                                                        
                                                    } else if let error = error {
                                                        print(error.localizedDescription)
                                                    }
        })
        task.resume()
    }
    
    func renderImage(_ image: UIImage, name: String) {
        let myView = UIView()
        let myImageView = UIImageView(image: image)
        myView.frame.size.width = imageWidth
        myView.frame.size.height = imageHeight
        myView.frame.origin.x = xPosition
        myView.frame.origin.y = yPosition
        myImageView.frame.size.width = imageWidth
        myImageView.frame.size.height = imageHeight
        myImageView.frame.origin.x = 0
        myImageView.frame.origin.y = 0
        myView.addSubview(myImageView)
        myView.accessibilityLabel = name
        let deleteButton = UIButton(frame: CGRect(x: imageWidth / 4.0 + 32, y: -(imageWidth / 4.0) - 32, width: imageWidth, height: imageHeight))
        deleteButton.setTitle("X", for: .normal)
        deleteButton.addTarget(self, action: #selector(deleteImage(_:)), for: .touchUpInside)
        myView.addSubview(deleteButton)
        
        let blurEffect = UIBlurEffect(style: UIBlurEffectStyle.dark)
        let blurView = UIVisualEffectView(effect: blurEffect)
        blurView.frame = myImageView.frame
        myImageView.addSubview(blurView)
        
        let blurButton = UIButton(frame: CGRect(x: 0, y: 0, width: imageWidth, height: imageHeight))
        blurButton.setTitle("Show Image", for: .normal)
        blurButton.addTarget(self, action: #selector(unBlur(_:)), for: .touchUpInside)
        myView.addSubview(blurButton)
        
        scrollView.addSubview(myView)
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

