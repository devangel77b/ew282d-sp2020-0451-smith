%% Area Calculations of Leaves
% This script uses color thresholding and blob labeling to find the areas
% of leaves exposed to wind. To do this, distinctly colored sheets of
% construction paper were placed behind the leaves and photographed. Then,
% the leaves were removed and the paper was photographed again. Using known
% dimensions of the paper, the area of leaves could be found by calculating
% the ratio of exposed to unexposed paper area. Images used for compariseon
% were taken from approximately the same distance away.
% Written by 3/C Cameron Smith on May 16, 2020.


%% Model Leaf
% Import image of leaf model against orange construction paper.
I=imread('MetalLeaf.jpg');
% Create binary image of leaf where only gray is initialized as 1's
[BW]=LeafMask(I);
% Label 8-bit connected blobs in the image
[L]=bwlabel(BW,8);
% Create a black image of same dimensions
N=zeros(3024, 4032);
% Manually select the largest blob (127), which must correspond to the leaf model
% Add this blob to the blank image, effectively eliminating other blobs
D=N+(L==127);
% Compute filled area of leaf, in pixels
leafsize=regionprops(D,'FilledArea');
%imshow(D)

%% Blank Paper
% Repeat process from above using image of construction paper without leaf
I=imread( 'Paper.jpg');
% Use different function for thresholding, find orange area
[BW]=PaperMask(I);
[L]=bwlabel(BW,8);
N=zeros(3024, 4032);
D=N+(L==1);
papersize=regionprops(D,'FilledArea');

%% Fan Area
% Find area of fan, which must be the cross sectional area exposed to wind
% Import image of construction paper which was same area as a box fan
% opening, set against a blue background.
I=imread( 'Fan1.jpg');
% Create binary image with black initialized as 1's
[BW]=Fan1Mask(I);
% Label 8-bit wise connected blobs
[L]=bwlabel(BW,8);
% Create black image with same dimensions
N=zeros(655, 491);
% After manually selecting the paper-shaped blob, set blob over the blank
% background
D=N+(L==12);
% Calculate filled area of blob
fan1size=regionprops(D,'FilledArea');
%% Fan Area 2
% Only leaf area exposed to wind is relevant, so find exposed box fan area
% with plant set in front of it. The difference between this and the actual
% box fan area is the exposed leaf area, in pixels.
% Import image of fan with black paper covering its opening and a
% grapefruit plant in front of it.
I=imread( 'Fan.jpg');
% Use thresholding function to create binary image, with black initialized
% as 1's.
[BW]=FanMask(I);
% Label 8-bit wise connected blobs and count number of blobs.
[L, num]=bwlabel(BW,8);
% Create black image of same dimensions.
N=zeros(809, 918);
% Create vector with an element for each blob.
k=zeros(1,num);
for i=1:num
    % Save the value of all blobs with areas greater than 100 pixels.
    D=N+(L==i);
    if(sum(sum(D)))>100
        k(i)=i;
    end
end
% Manually select blobs which are false positives (the outer rim of box fan
% instead of opening) and set equal to zero.
k([55 61 66])=0;
D=N;
% Put blobs together on a blank background to inspect.
for i=1:num
    if( k(i)~=0)
        D=D+(L==k(i));
    end
end
% View blobs.
imshow(D)
% Initialize variable for the exposed fan area.
fansize=0;
for i=1:num
    % For any valid blob,
    if( k(i)~=0)
        % Initialize blob on blank background.
        D=N+(L==k(i));
        % Calculate filled blob area.
        data=regionprops(D,'FilledArea');
        % Add blob area to running total.
        fansize=fansize+data.FilledArea;
    end
end

%% Calculations
% Calculate known area of orange rectangular construction paper, in square feet.
paperarea=9*12/144;
% Calculate known area of black, fan-shaped construction paper, in square
% feet.
blackarea=8.5^2*pi/(4*144);
% The area of the model leaf is leaf size (in pixels)/paper size (in
% pixels)*actual paper size. Effectively, find the percentage of the paper
% covered by the leaf.
metalarea=leafsize.FilledArea/papersize.FilledArea*paperarea;
% Repeat process to find percentage of fan that ramained exposed, then
% subtract from the known fan area to find the leaf area.
grapefruitarea=blackarea-blackarea*fansize/fan1size.FilledArea;